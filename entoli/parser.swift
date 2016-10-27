//
//  parser.swift
//  entoli
//
//


private let DEBUG = false


// TO DO: commas should create `.sentence` ExpressionBlock, e.g. `To stuff {} this, that. Other.` should parse as `func stuff(){this;that};other;`, but currently parse as `func foo(){this};that;other;`. Note that a list of expression blocks also needs to be supported, e.g. `This. That. Other`, though whether a single ExpressionBlock should support both needs to be determined (from the parser's POV, it's probably a case of appending everything up to `.` to the previous item, so if previous item is `to stuff{}` then `this,that.` gets attached to that, which then gets added to the parent expression block followed by `Other.` That should keep the AST shallow while doing the right thing with right operands without tight coupling to left operands.


// TO DO: if period-delimited expression lists are used for short-form (single-line) blocks, and `do...done` for long-form (multi-line) blocks, is there still a reason to support parenthesized expression lists for blocks as well? It would simplify language and eliminate potential source of errors if parens were used for grouping single expressions (e.g. math operators) only. (Note: argument for parensed expression lists is that it allows arbitrary length, nestable groups using built-in punctuation tokens only, whereas `do...done` blocks use keywords which are imported.) Note that a comma/semicolon-separated block is still a single expression [arguably a dodgy concept since only semicolons actually represent composition; commas don't], so parenthesizing it doesn't really change semantics; it just makes the grouping explicit (and eliminates the need for an explicit trailing period). The question then becomes whether period separators should be allowed too, as those would describe a sequence of block expressions, e.g. `(Foo, bar. Baz)`, which does then require parens to be blocks and not just grouping. Mostly a question of principle of least surprise. The worry is that commands could inadvertently (or deliberately) appear within a parensed expression without actually affecting its output, though might still act in other ways (e.g. side-effects) when that expression is evaluated, e.g. `(die, 2 + 2) * 3`. [Technically, any command in any imperative language could perform the same side-effects, e.g. `func twoPlusTwo() {die; return 2 + 2}; twoPlusTwo() * 3;` - the only difference being is it happens inside the procedure rather than its call site.]


// TO DO: +/- numeric operator parsing is currently a bit unintuitive when white space omitted on both sides, e.g. `1+5` parses as `1. +5.` instead of `'+' {1, 5}`; not sure how best to deal with this; e.g. auto-correct should at least flag it as suspect/query user when the left side is a numeric, or insert a space before symbol when left side is a name. (The logic's more apparent when witten as `n -1`, as `-1` can be read as the argument to the `n` command; if `n` is already defined then auto-correct can see if it takes an argument and/or returns a value and better refine its suggested/recommended corrections.)

// TO DO: need smarter parsing of +/- [and other overloaded prefix+infix] operators when LH operand is a command without an argument, e.g. `x + y`, in which case it should parse as `(x) + (2)`, not `x {+(2)}` as it currently does; e.g. have parseArgument() check for a potential infix operator after the command name and, if found, backtrack and return arg-less command, leaving parseOperation() to pick up infix operator as normal


//**********************************************************************


class Parser {
    
    let lexer: Lexer // operator parsefuncs also call lexer methods to read their operand tokens, but must leave it in correct state when done
    
    init(lexer: Lexer) {
        self.lexer = lexer
    }
    
    //**********************************************************************
    // TOKENS
    
    private var precedenceForNextToken: Int { // used by parseOperation() to determine if leftExpr binds more tightly to its right (i.e. the infix/postfix operator [if any] currently being processed) than its left (the previous prefix/infix operator [if any])
        // TO DO: this gets a bit thorny with non-breaking whitespace tokens (that said, linebreaks should always delimit exprs; the only ones to worry about are in groups) (Q. what about linebreaks/comments/annotations?); lookahead should always skip those
        let token = self.lexer.lookaheadBy(1)
        if token.type == .endOfCode { return Int.min } // break loop
//        print("precedenceForNextToken: \(token)")
        let precedence = token.type.precedence
        if precedence == gOperatorDefinedPrecedence {
            return token.infixOperator?.precedence ?? 0 // only infix/postfix ops are of relevance (atom/prefix ops do not take a left operand [i.e. leftExpr], so return 0 for those to finish the previous expression and start a new one)
        } else {
            return precedence
        }
    }
    
    
    //**********************************************************************
    // PARSE RECORD
     
    // TO DO: disallow numericals as record names as they should _always_ be treated as a hardcoded special form (as opposed to operators, which are extensible special form), e.g. {12:00} is a time value, not a name:value pair; note that `{a 0:...}` and `{0a:...}` should probably also be disallowed (or at least discouraged) as those names do not transfer well to command scope (requiring single-quoted to be read as names, not command/numeric unit respectively); probably just requires tweaking Lexer.readUnquotedName() so that readNextToken(ignoreVocabulary:true) only ignores operators, not numerics

    
    // TO DO: parseRecord could optionally be parameterized with parsefunc that knows how to read a record item; that'll allow context-sensitive interpretation; as for `to` op vs `to` command, don't need to worry about latter as typespecs will insist on NAME or NAME:VALUE, forcing users to single-quote as needed (note: procs are typically defined at top-level, so any malformed records will be reported as soon as script runs; thus parse-time checking, while nice, isn't essential)
    
    // parse record, checking items are `NAME-LITERAL:ANY-VALUE` pairs and/or `ANY-VALUE` expressions
    private func parseRecord() throws -> Value { // TO DO: allow optional validation func to be passed in, e.g. `to` op needs to apply additional restrictions on item types (`NAME-LITERAL:TYPE-COMMAND` and/or `NAME-LITERAL` only)
        var result = [Value]()
        while true { // starts on `{`
            let backtrackIndex = self.lexer.currentTokenIndex
            self.lexer.advance(ignoreVocabulary: true) // initially try reading as quoted/unquoted name followed by pair separator (i.e. a named record item)
            let token = self.lexer.currentToken
 //           print("parseRECORD: \(token)")
            var isNamedPair: Bool = false
            switch token.type { // note: if item looks like a valid `name:value pair`, need to check pairs' precedence to make sure infix/postfix operators never bind entire pair where they're really intended to bind RH operand only (if they do bind entire pair, it shouldn't break following logic, but it will confuse users since what looks like a named arg is actually a positional one; possibly the safest option is for all operators to have higher precedence than punctuation, and enforce this in OperatorTable; though we will need to watch for 'stray' punctuation getting sucked up in LH operand when it's supposed to delimit it)
            case .recordLiteralEnd:
                return Record(result) // double-check this leaves us at correct position
            case .endOfCode:
                throw EndOfCodeError(description: "[1a] End of code.")
            case .quotedName, .unquotedName: // item is of form `'NAME':...` or `NAME:...` 
                // (note: multiple names/aliases c.f. Swift funcs are not supported as that would create confusion when pairs are used as `store` shortcuts in command eval scopes as that's analogous to Swift's multiple assignment, and having different binding rules for different contexts will confuse users)
                isNamedPair = self.lexer.lookaheadBy(1).type == .pairSeparator // literal pairs in records MUST have literal name as LH operand (note: this looks ahead 2, since currentToken is `{` and next token is QuotedName)
            default:
                isNamedPair = false
            }
            let value: Value
            if isNamedPair {
                value = try self.parseOperation(Name(token.value))
            } else {
 //               print("...backtrack from \(self.lexer.currentTokenIndex): (\(self.lexer.currentToken))")
                self.lexer.backtrackTo(backtrackIndex, flush: true)
 //               print("            ...to \(backtrackIndex): \(self.lexer.currentToken)")
 //               print("==============\n\t",self.lexer.currentTokensCache.map{String($0)}.joinWithSeparator("\n\t"), "\n===============")
                value = try self.parseExpression()
 //               print("READ ITEM:", value)
            }
            // additional validity checks
            if (type(of: value) == Pair.self) != isNamedPair { // note: this also disallows `name:value` pair if it's subsequently parsed as LH operand to a lower-precedence operator (technically, this would be a legal positional value, but visually it would be confusing so we disallow it; if users want to apply a low-precedence operator to pair's RH value, they'll need to explicitly parenthesize it)
                throw SyntaxError(description: "Bad record item (pairs within records must have a literal name): \(value)")
            }
            result.append(value)
            //print("CURR: \(self.lexer.currentToken)\n")
        }
    }
    
    
    //**********************************************************************
    // PARSE COMMAND ARGUMENT
    
    private func parseArgument(_ name: Name) throws -> Value {
        // called by parseAtom after parsing a quoted/unquoted name
        // given a Name, return a CommandValue if it's followed by a valid argument, otherwise return Name unchanged
        // note: command name and argument will always bind more tightly to each other than to operators
        // note: this is right-associative, so `'foo' 'bar' 'baz'` will parse as `foo {bar {baz}}`
        if DEBUG {print("parseArgument (if any) for name: \(name)")}
        let backtrackIndex = self.lexer.currentTokenIndex
        var argument: Value?
        do {
            argument = try self.parseAtom() // this'll throw EOF error if command name is last
        } catch is EndOfCodeError {
            argument = nil
        } catch is LeftOperandNotFoundError { // parseAtom() found an infix operator; return name and let parseOperation deal with it
            argument = nil
        }
        if argument == nil { // name is not followed by a new expression, so it is either just a name or an argument-less command; in either case, return it unchanged as Name // TO DO: problem with this is that in an expr[seq] context it needs to eval as Command
            self.lexer.backtrackTo(backtrackIndex)
            if DEBUG {print("... no argument found, so parseArgument backtracked to \(self.lexer.currentToken)")}
            return name // TO DO: if next token is `do...done` block (a postfix structure), confirm that this gets attached to command correctly (note that its parsefunc will need to cast name to command first)
        } else { // TO DO: also, what if `nothing` literal is found?
           
            if let expr = argument as? ExpressionBlock { // note: if argument literal is `()` (which is synonymous with `nothing`, as that's what it returns when evaled, and not something the user would intend here as it's simpler just to omit the argument entirely), the parser automatically 'corrects' it by replacing it with an empty record (which is also equivalent to omitting argument entirely). This should avoid runtime errors due to users habitually typing `foo()` instead of `foo{}` (note: to explicitly pass nothing, use `foo{nothing}`, which is functionally identical to `foo{}` or `foo`). OTOH, `foo(1)`, `foo(1,2,3)` is not 'auto-corrected' as its intent is ambiguous (the user may have intended it to be a group whose result is passed as single argument to `foo`), but will be visibly incorrect when formatted as `foo{(1)}`, `foo{(1,2,3)}`; it might also be worth parser/formatter flagging it as questionable when performing a syntax check (though am inclined not to do that in parser itself)
                if expr.expressions.count == 0 { argument = gEmptyRecord }
            }
            if !(argument is Record) { argument = Record([argument!]) } // non-record values are treated as first item in a record arg
            // any pairs in argument record *must* have Name as LH operand (in theory, they could be treated as positional if LH is non-name, but this will likely cause user confusion), so check and throw syntax error if not (e.g. to pass a pair as a positional arg, just wrap in parens, though bear in mind how it's evaled will depend on context)
            for (i, item) in (argument as! Record).fields.enumerated() {
                if item is Pair && !((item as! Pair).key is Name) {
                    throw SyntaxError(description: "Malformed record argument for \(name) command: item \(i+1) is a name-value pair, but its left side is not a literal name: \(item)")
                }
            }
            return Command(name: name, argument: argument!) // TO DO: if next token is a postfix `do...done` block, confirm that this gets attached to command correctly (i.e. its parsefunc should coerce its LH operand to Command, then add parsed block to it)
        }
    }
    
    
    //**********************************************************************
    // PARSE EXPRESSION
    
    func parseAtom(_ precedence: Int = 0) throws -> Value? { // parse atom or prefix op (i.e. no left operand)
        let previousIndex = self.lexer.currentTokenIndex
        self.lexer.advance() // TO DO: confirm this always considers vocab
        if DEBUG {print("\nPARSE_ATOM advanced from \(previousIndex) to \(self.lexer.currentTokenIndex): \(self.lexer.currentToken)")}
        let token = self.lexer.currentToken
        if token.type == .endOfCode { throw EndOfCodeError(description: "[1] End of code.") } // outta tokens
        switch token.type {
            // PUNCTUATION TOKENS
        case .annotationLiteral: // «...» // attaches arbitrary contents to subsequent node as metadata
            let annotation = token.value
            let value = try self.parseExpression(TokenType.annotationLiteral.precedence) // bind like glue
            value.annotations.append(annotation)
            return value
            // note: rest of these are atoms (no ops/precedence)
        case .listLiteral: // [...];  an ordered collection (array) or key-value collection (dictionary)
            var result = [Value]()
            while self.lexer.lookaheadBy(1).type != .listLiteralEnd {
                let value = try self.parseExpression()
                result.append(value)
            }
            try self.lexer.skip(.listLiteralEnd) // TO DO: skip is only really needed to throw error if code terminates without closing `]`
            return List(items: result)
        case .recordLiteral: // {...}; a sequence of values and/or name-value pairs; mostly used to pass proc args
            return try self.parseRecord()
        case .expressionSequenceLiteral: // (...) // a sequence of zero or more expressions, grouped by parentheses; also be aware that `(x:1)` will be treated as an ordinary pair, not a `store` command, unlike in (e.g.) `do...done` blocks
            let result = ExpressionBlock(expressions: try self.parseExpressionSequence({$0.type == .expressionSequenceLiteralEnd}), format: .parenthesis)
            try self.lexer.skip(.expressionSequenceLiteralEnd)
            return result
            // atomic literals
        case .quotedText: // "..." // a text (string) literal
            return Text(token.value)
        case .quotedName: // '...' // an explicitly delimited name literal
            // if next significant token is another expr, treat it as an RH operand (i.e. argument) and emit CommandValue, else return Name
            return try self.parseArgument(Name(token.value)) // speculatively parses for a command argument after the name; if none is found, backtracks to the name token and returns it as-is
            // VOCABULARY TOKENS
        case .numericWord:
            guard let numericInfo = token.numericInfo else { throw SyntaxError(description: "BUG: Numeric data is missing") } // note: this should never happen
            switch numericInfo {
            case .invalid(let word, let error):
                throw SyntaxError(description: "Malformed numeric literal (\(error)): `\(word)`")
            case .utf8EncodedString(let string): // UTF8-encoded text literal, `0u...`
                let result = Text(string)
                result.annotations.append(token.value) // TO DO: might be better just to indicate text value was created from an `0u...` literal, and leave formatter to generate "0u..." representation if/when needed
                return result
            default:
                let result = Text(token.value)
                result.annotations.append(numericInfo) // TO DO: how to implement annotation API?
                return result
            }
        case .unquotedName:
            return try self.parseArgument(Name(token.value)) // speculatively parses for a command argument after the name; if none is found, backtracks to the name token and returns it as-is
        case .operator:
            if DEBUG {print("\nparseAtom got Operator: \(token.prefixOperator) \(token.infixOperator)")}
            guard let operatorDefinition = token.prefixOperator else {
                self.lexer.backtrackTo(previousIndex)
                assert(token.infixOperator != nil, "BUG in Parser.parseAtom(): .Operator token contains neither prefix nor infix definition: \(token)")
                throw LeftOperandNotFoundError(description: "parseAtom() encountered an infix operator: \(token.infixOperator!)") // note: when speculatively parsing a command argument, this indicates the command has no argument so will be used as leftExpr; elsewhere it's a syntax error
            }
            let result = try operatorDefinition.parseFunc(self, operatorDefinition.name.text, precedence)
            if DEBUG {print("... and returned it: \(result)")}
            return result
            // OTHER
        case .lineBreak: // a line break acts as an expression separator
            return nil
            // INVALID TOKENS (these cannot appear where an atom or prefix operator is expected)
        default:
            // AnnotationLiteralEnd, RecordLiteralEnd, ListLiteralEnd, ExpressionSequenceLiteralEnd will only appear if opening '«', '{', '[', or '(' token is missing, in which case parser should treat as syntax error [current behavior] or as an incomplete structure in chunk of code whose remainder is in a previous chunk [depending how incremental parsing is done]; any other token types are infix/postfix ops which are missing their left operand
            return nil // TO DO: this should throw, and .LineBreak should be treated separately
        }
    }
    
    func parseOperation(_ leftExpr: Value, precedence: Int = 0) throws -> Value { // parse infix/postfix
        var leftExpr = leftExpr
        if DEBUG {print("\n\nENTERED parseOperation on \(self.lexer.currentTokenIndex), leftExpr=(\(leftExpr), \(precedence)) \n\t\ttoken=\(self.lexer.currentToken)\n\t\tnext=\(self.lexer.lookaheadBy(1))\n\n")}
        while precedence < self.precedenceForNextToken {
            let previousIndex = self.lexer.currentTokenIndex
            self.lexer.advance()
            if DEBUG {print("\nPARSE_OPER: advanced from \(previousIndex) to \(self.lexer.currentTokenIndex)\n\t\ttoken=\(self.lexer.currentToken)")}
            let token = self.lexer.currentToken
            if token.type == .endOfCode { throw EndOfCodeError(description: "[2] End of code.") } // outta tokens
            switch token.type {
                // PUNCTUATION TOKENS
            case .annotationLiteral: // «...» // attaches arbitrary contents to preceding node as metadata
                leftExpr.annotations.append(token.value)
            case .expressionSeparator: // period separator
                return leftExpr // as with linebreak token, period token (`.`) indicates end of expression (e.g. `1. - 2` is two expressions, not an operation), so is a no-op
            case .itemSeparator: // comma separator is normally used to separate list and record items; here it acts as an expression separator, though period separators are strongly preferred to avoid confusion // TO DO: in command contexts, can/should commas work as separators within an unparenthesized expression group, with periods/linebreaks terminating the group? (in practice, we don't want to create more groups than needed)
                return leftExpr
            case .pairSeparator: // found colon separator for key:value pair
                leftExpr = Pair(leftExpr, try self.parseExpression(TokenType.pairSeparator.precedence-1))
            case .pipeSeparator: // found semicolon separator, indicating leftExpr should be used as first field in RH command's arg list, e.g. given `foo{1};bar{2}`, transforms it into `bar{foo{1},2}`, annotating `foo` command with formatting preference (i.e. `;` should be purely syntactic sugar, with no semantic representation); also note that rightExpr must be a command (possibly in parens) and `;` needs to bind to it like glue, much as command-arg pairs already do (i.e. no operator should have higher precedence)
                // TO DO: if RH expr is an operator, it already has all its [leading] arguments; thus, the preceding `;` must be reported as a syntax error; when creating command from operator, might want to annotate it so that no more ops can be added [prefixed?] (caveat do block suffix?), or just annotated to indicate it was created from an operator literal, allowing that annotation to be checked for here and an error raised if found.
                let rightExpr = try self.parseExpression(TokenType.pipeSeparator.precedence)
                let command: Command
                do {
                    command = try rightExpr.toCommand()
                } catch {
                    throw SyntaxError(description: "Expected command after `;` but found \(rightExpr.typename): \(rightExpr)")
                }
                // TO DO: would be better to call `insertArgument` on Commmand and let it return new Command instance, or throw error if it knows it has all its args (i.e. operator-created)
                leftExpr = Command(name: command.name, argument: Record([leftExpr] + command.argument.fields))
                leftExpr.annotations.append("Format.PipedInput") // TO DO: suspect annotations will be dict with [mostly] constant keys, probably grouping by purpose (e.g. `gPipedArgument` flag would be added to `gFormatting` set/sub-dict; the only caveat being that nested dicts, while easier to view/search for arbitrary keys, add complexity to implementation... TBH, one could just about argue for using a linked list)
                leftExpr.annotations.append(contentsOf: command.annotations)
                // VOCABULARY TOKENS
            case .operator:
                if DEBUG {print("\nparseOperation got Operator: \(token.prefixOperator) \(token.infixOperator)")}
                guard let operatorDefinition = token.infixOperator else { // found a prefix operator, so end this expression and process it on next pass
                    assert(token.infixOperator != nil, "BUG in Parser.parseOperation(): .Operator token contains neither prefix nor infix definition: \(token)")
                    self.lexer.backtrackTo(previousIndex)
                    if DEBUG {print("... and returned leftExpr: \(leftExpr)")}
                    return leftExpr
                }
                leftExpr = try operatorDefinition.parseFunc(self, leftExpr, operatorDefinition.name.text, operatorDefinition.precedence)
                // OTHER
            case .lineBreak: // a line break always acts as an expression separator
                return leftExpr // TO DO: need to check this is correct // TO DO: check behavior is appropriate when linebreak appears between an operator and its operand[s]; also make sure it behaves correctly when it appears after a pipe (semi-colon) separator, as it's not unreasonable for the RH operand to appear on the following line in that case
                // INVALID TOKENS (these cannot appear where an infix/postfix operator is expected)
            default: // anything else implictly starts a new expression (if token is invalid, e.g. unbalanced bracket/brace/paren, parseAtom will detect it and throw error on next pass)
                self.lexer.backtrackTo(previousIndex)
                return leftExpr
            }
        }
        return leftExpr
    }
    
    
    //**********************************************************************
    // parse a single expression (i.e. a single atom or prefix operation, followed by zero or more infix and/or postfix operations on it)
    
    
    func parseExpression(_ precedence: Int = 0) throws -> Value {
        while self.lexer.lookaheadBy(1).type == .lineBreak { // skip any line breaks prior to new expression (note: these will need preserved for display purposes; perhaps as annotations?)
            print("parseExpression skipping linebreak")
            self.lexer.advance()
        }
        if DEBUG {print("\n\n[START] parseExpression at \(self.lexer.currentTokenIndex): \(self.lexer.lookaheadBy(1))")}
        guard let leftExpr = try self.parseAtom(precedence) else {
            throw SyntaxError(description: "[1] Unexpected \"\(self.lexer.currentToken)\"")
        }
        if DEBUG {print("[NEXT]  parseExpression: \(leftExpr)\n")}
        let result = try self.parseOperation(leftExpr, precedence: precedence)
        if DEBUG {print("[DONE]  parseExpression: \(result)\n")}
        return result
    }
    
    
    
    // TO DO: what about .LineBreak?
    
    func parseExpressionSequence(_ isEndToken: ((Token) -> Bool), isBlock: Bool = false) throws -> [Value] {
        // note: this method reads up to, but does not advance onto, the end token; caller must do that on return
        // isBlock should be true only if the caller wants named pairs to be treated as syntactic shortcuts for `store` commands (e.g. `x:1` -> `store{value:1,named:x}`)
        var result = [Value]()
        while !isEndToken(self.lexer.lookaheadBy(1)) {
            var value = try self.parseExpression()
            if let command = try? value.toCommand() {
                value = command
            } else if isBlock && value is Pair {
                if let command = try? (value as! Pair).toStoreCommand() { value = command }
            }
            result.append(value)
            self.lexer.flush() // TO DO: check this is OK
        }
        return result
    }
    
    
    //**********************************************************************
    // PARSE SCRIPT
    
    
    func parseScript() throws -> EntoliScript { // parse entire document (result is expression group)
        let result = try self.parseExpressionSequence({$0.type == .endOfCode}, isBlock: true) // TO DO: check this works ok
        if DEBUG {print("TOP-LEVEL parse() completed expr: \(result.last)")}
        return EntoliScript(expressions: result)
    }
}

