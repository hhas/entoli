//
//  parser.swift
//  entoli
//
//


// note: on balance, I think making commands' bind infinitely tightly is probably best, as it's an easy rule for users to remember



// TO KEEP SYNTAX RULES SIMPLE: numbers and units must start with optional `-`/`+` prefix followed by one of `0123456789`, and cannot contain whitespace, and they ALWAYS self-delimit.
// (this leaves currency to be decided, probably by allowing currency symbols as additional prefixes)

// TO DO: any reason why DDs can't apply themselves as symbol operator parsefuncs?

// Q. what is relationship between type specifiers (run-time coercions, specifically those that coerce from text to Swift primitives) and data detectors (parse-time readers)? bear in mind that if a DD can read a string, it can read either code or Text, so there's probably a lot of commonality in their core behavior

// Q. are there any use-cases where DDs wouldn't start with number (specifically, one of `-+0-9`)? (e.g. 42, 3.5kg, -123e4, +0.9, 0x1A2, 0u000A, 2015-06-12, 12:22:45 +0100)? Currency ($9.99) is one, any others?

// Q. should numbers be detected in lexer? or can/should they be handled as part of unquoted word parsing (after all, it analyzes each word as it goes, and will be doing so in detail once in-word symbol operator matching is also implemented)? -- still think numbers might be matchable using symbols approach, though might require a little more flexibility as it does need ability to reach out and lookahead/parse additional tokens, e.g. periods with no surrounding whitespace and one or more digits thereafter (i.e. decimal number) -- TBH, it's a bit like per-char parsing needs ability to specify scanning fixed or open-ended ranges of digits, with additional parsefuncbeing called at end depending on what's next


// TO DO: how to parameterize keywordOperators? (e.g. should there be standard ops tables that are cloned by init each time a new Parser instance is created; this still leaves question of how to add/remove opdefs, which is probably something that needs to be done in top-level parse() loop as it has limited ability to analyze/execute completed top-level expressions)




// note: there's probably not much point worrying about preserving original whitespace vs automatically cleaning it up until code is sufficiently complete to allow testing; for now, just ignore issue entirely (it'll simplify implementation since e.g. parseName won't have to preserve original code substrings as well as generate canonical names - plus bear in mind that capturing code ranges in Values is useless in practice as any code edit invalidates all subsequent ranges)

// next problem: using recursion likely makes incremental parsing problematic


// note: another possibility for matching ops defined as both prefix and infix/postfix (e.g. `-`) is to rely on proc overloading, and have record arg with explicit keys (this'd avoid defining disambiguated names, e.g. `neg`, that aren't already visible, though arguably is a misuse of overloading in that behaviors are not simply variations on each other but quite different, e.g. negation vs subtraction)


// TO DO: when parsing ListLiteral, if all items are pairs, make this an associative list (note: need to decide rules by which ordered lists [Array], associative lists [Dict], and unique lists [Set] are tagged and/or coercion-locked as that type; this will largely be determined by usage, e.g. once an array operation is performed, the list should thereafter act as array and refuse to coerce to dict or set - though explicit casting should still be allowed; of course, a lot depends on whether lists are mutable or immutable, and that policy/mechanism has yet to be decided)



private let DEBUG = true



class SyntaxError: ErrorType, CustomStringConvertible {

    let description: String
    
    init(description: String = "Syntax error.") { // TO DO: also needs range + source
        self.description = description
    }

} // TO DO: how to store error info? (ideally, should be NSError-compatible without being dependent on it)

class EndOfCodeError: SyntaxError {}

class LeftOperandNotFoundError: SyntaxError {}


/**********************************************************************/


class Parser {
    // TO DO: currently whitespace tokens are always included in lexer stream; if they usually aren't needed (e.g. to disambiguate adjoining tokens) then lexer could be revised to emit significant tokens only (alternatively, if lookahead and backtracking moves to lexer, could use 'includeWhiteSpace' arg in VocabularyLexer.lookahead() to check for whitespace separator only when it needs them, e.g. when parsing multi-word unquoted names)
    
    let lexer: Lexer
    
    init(lexer: Lexer) {
        self.lexer = lexer
    }
    func parseExpression(precedence: Int = 0) throws -> Value { // parse atom or prefix op, followed by any infix/postfix ops
        throw SyntaxError(description:"TODO")
    }

    /**********************************************************************/
    // TOKENS
    
    // TO DO: need to maintain cache of punctuation+vocabulary tokens
    
    var currentToken: Token? { return self.lexer.currentToken } // current token
    
    /*
    private var precedenceForNextToken: Int { // used by parseOperation() to determine if leftExpr binds more tightly to its right (i.e. the infix/postfix operator [if any] currently being processed) than its left (the previous prefix/infix operator [if any])
        // TO DO: this gets a bit thorny with non-breaking whitespace tokens (that said, linebreaks should always delimit exprs; the only ones to worry about are in groups) (Q. what about linebreaks/comments/annotations?); lookahead should always skip those
        guard let token = self.lexer.lookaheadBy(1) else { return Int.min } // break loop
//        print("precedenceForNextToken: \(token)")
        let precedence = token.type.precedence
        if precedence == gOperatorDefinedPrecedence {
            return token.infixOperator?.precedence ?? 0 // only infix/postfix ops are of relevance (atom/prefix ops do not take a left operand [i.e. leftExpr], so return 0 for those to finish the previous expression and start a new one)
        } else {
            return precedence
        }
    }
    
    
    /**********************************************************************/
    // utility funcs; used to clean up collection items
    
    private func stripComma(value: Value) -> Value { // used to tidy list items after parsing; TO DO: should this strip _all_ commas? or treat adjoining commas as syntax error/warning (e.g. [1,2,,4] is legal but suggests a typo)? or just leave as-is and leave user to clean up surplus commas in code if it bothers them
        if let v = value as? ItemSeparator { return v.data }
        return value
    }
    
    private func stripPeriod(value: Value) -> Value { // TO DO: ditto
        if let v = value as? ExpressionSeparator { return v.data }
        return value
    }
    
    
    /**********************************************************************/
    // PARSE RECORD
    
    // TO DO: parseRecord could optionally be parameterized with parsefunc that knows how to read a record item; that'll allow context-sensitive interpretation; as for `to` op vs `to` command, don't need to worry about latter as typespecs will insist on NAME or NAME:VALUE, forcing users to single-quote as needed (note: procs are typically defined at top-level, so any malformed records will be reported as soon as script runs; thus parse-time checking, while nice, isn't essential)
    
    // parse record, checking items are `NAME-LITERAL:ANY-VALUE` pairs and/or `ANY-VALUE` expressions
    private func parseRecord() throws -> Value { // TO DO: allow optional validation func to be passed in, e.g. `to` op needs to apply additional restrictions on item types (`NAME-LITERAL:TYPE-COMMAND` and/or `NAME-LITERAL` only)
        var result = [Value]()
        while self.lexer.lookaheadBy(1)?.type != .RecordLiteralEnd {
            //     guard let nextTokenType = self.lexer.lookaheadBy(1)?.type else { throw EndOfCodeError(description: "[1a] End of code.") }
            guard let nextToken = self.lexer.lookaheadBy(1) else { throw EndOfCodeError(description: "[1a] End of code.") }
            let isLegalPair: Bool
            let value: Value
            switch nextToken.type { // note: if item looks like a valid `name:value pair`, need to check pairs' precedence to make sure infix/postfix operators never bind entire pair where they're really intended to bind RH operand only (if they do bind entire pair, it shouldn't break following logic, but it will confuse users since what looks like a named arg is actually a positional one; possibly the safest option is for all operators to have higher precedence than punctuation, and enforce this in OperatorTable; though we will need to watch for 'stray' punctuation getting sucked up in LH operand when it's supposed to delimit it)
            case .QuotedName: // check for `'NAME' :`
                isLegalPair = self.lexer.lookaheadBy(2)?.type == .PairSeparator // literal pairs in records MUST have literal name as LH operand (note: this looks ahead 2, since currentToken is `{` and next token is QuotedName)
                value = self.stripComma(try self.parseExpression())
            case .UnquotedWord: // check for [e.g.] `WORD WORD WORD :`
                let backtrackIndex = self.lexer.currentTokenIndex
                var words = [String]()
                repeat {
                    self.lexer.advance()
                    words.append(self.lexer.currentToken!.value)
                } while self.lexer.lookaheadBy(1, ignoreWhiteSpace: false)?.type == .WhiteSpace
                                              && self.lexer.lookaheadBy(1)?.type == .UnquotedWord
                isLegalPair = self.lexer.lookaheadBy(1)?.type == .PairSeparator
                if isLegalPair { // it's a pair with only words on left side, so treat those words as pair's name (i.e. no data detection or operator name matching is performed; even numbers are legal), and parse the rest of the pair
                    value = self.stripComma(try self.parseOperation(NameValue(data: words.joinWithSeparator(" "))))
                } else { // it's a non-pair expression
                    self.lexer.backtrackTo(backtrackIndex)
                    value = self.stripComma(try self.parseExpression())
                }
            default:
                isLegalPair = false
                value = self.stripComma(try self.parseExpression())
            }
            if value is PairValue && !isLegalPair || isLegalPair && !(value is PairValue) { // note: this also disallows `name:value` pair if it's subsequently parsed as LH operand to a lower-precedence operator (technically, this would be a legal positional value, but visually it would be confusing so we disallow it; if users want to apply a low-precedence operator to pair's RH value, they'll need to explicitly parenthesize it)
                throw SyntaxError(description: "Bad record item (pairs within records must have a literal name): \(value)")
            }
            result.append(value)
            //print("CURR: \(self.currentToken)\n")
        }
        try self.lexer.skip(.RecordLiteralEnd) // TO DO: skip is only really needed to throw error if code terminates without closing `]`
        return RecordValue(data: result)
    }
    
    
    /**********************************************************************/
    // PARSE COMMAND ARGUMENT
    
    private func parseArgument(name: NameValue) throws -> Value {
        // called by parseAtom after parsing a quoted/unquoted name
        // given a NameValue, return a CommandValue if it's followed by a valid argument, otherwise return NameValue unchanged
        // note: command name and argument will always bind more tightly to each other than to operators
        // note: this is right-associative, so `'foo' 'bar' 'baz'` will parse as `foo {bar {baz}}`
        print("parseArgument (if any) for name: \(name)")
        let backtrackIndex = self.lexer.currentTokenIndex
        var argument: Value?
        do {
            argument = try self.parseAtom() // this'll throw EOF error if command name is last
        } catch is EndOfCodeError {
            argument = nil
        } catch is LeftOperandNotFoundError { // parseAtom() found an infix operator; return name and let parseOperation deal with it
            argument = nil
        }
        if argument == nil { // name is not followed by a new expression, so it is either just a name or an argument-less command; in either case, return it unchanged as NameValue
            self.lexer.backtrackTo(backtrackIndex)
            print("... no argument found, so parseArgument backtracked to \(self.currentToken)")
            return name // TO DO: if next token is `do...done` block (a postfix structure), confirm that this gets attached to command correctly (note that its parsefunc will need to cast name to command first)
        } else {
            if !(argument is RecordValue) { argument = RecordValue(data: [argument!]) } // non-record values are treated as first item in a record arg
            // any pairs in argument record *must* have NameValue as LH operand (in theory, they could be treated as positional if LH is non-name, but this will likely cause user confusion), so check and throw syntax error if not (e.g. to pass a pair as a positional arg, just wrap in parens, though bear in mind how it's evaled will depend on context)
            for (i, item) in (argument as! RecordValue).data.enumerate() {
                if item is PairValue && !((item as! PairValue).name is NameValue) {
                    throw SyntaxError(description: "Malformed record argument for \(name) command: item \(i+1) is a name-value pair, but its left side is not a literal name: \(item)")
                }
            }
            return CommandValue(name: name, data: argument!) // TO DO: if next token is a postfix `do...done` block, confirm that this gets attached to command correctly (i.e. its parsefunc should coerce its LH operand to Command, then add parsed block to it)
        }
    }
    
    
    /**********************************************************************/
    // PARSE EXPRESSION
    
    private func parseAtom(precedence: Int = 0) throws -> Value? { // parse atom or prefix op (i.e. no left operand)
        let previousIndex = self.lexer.currentTokenIndex
        self.lexer.advance() // TO DO: confirm this always considers vocab
        if DEBUG {print("\nPARSE_ATOM advanced from \(previousIndex) to \(self.lexer.currentTokenIndex): \(self.lexer.currentToken ?? nil)")}
        guard let token = self.lexer.currentToken else { throw EndOfCodeError(description: "[1] End of code.") } // outta tokens
        switch token.type {
            // PUNCTUATION TOKENS
        case .AnnotationLiteral: // «...» // attaches arbitrary contents to subsequent node as metadata
            let annotation = token.value
            let value = try self.parseExpression(TokenType.AnnotationLiteral.precedence) // bind like glue
            value.annotations.append(annotation)
            return value
            // note: rest of these are atoms (no ops/precedence)
        case .ListLiteral: // [...];  an ordered collection (array) or key-value collection (dictionary)
            var result = [Value]()
            while self.lexer.lookaheadBy(1)?.type != .ListLiteralEnd {
                let value = self.stripComma(try self.parseExpression()) // parser treats comma separator as postfix op to preserve it, but collections are self-delimiting so don't need it
                result.append(value)
            }
            try self.lexer.skip(.ListLiteralEnd) // TO DO: skip is only really needed to throw error if code terminates without closing `]`
            return ListValue(data: result)
        case .RecordLiteral: // {...}; a sequence of values and/or name-value pairs; mostly used to pass proc args
            return try self.parseRecord()
        case .ExpressionGroupLiteral: // (...)
            var result = [Value]()
            while self.lexer.lookaheadBy(1)?.type != .ExpressionGroupLiteralEnd {
                let value = self.stripPeriod(try self.parseExpression())
                result.append(value)
            }
            try self.lexer.skip(.ExpressionGroupLiteralEnd)
            return ExpressionGroupValue(data: result)
        case .LineBreak: // linebreaks act as name (and expr?) separators
            return try self.parseExpression() // skip linebreak and parse next line as new expression // TO DO: prob. should preserve linebreak (e.g. as postfix no-op) for display purposes (i.e. reformatting user's code is one thing; reformatting their layout is another)
            // atomic literals
        case .QuotedText: // "..." // a text (string) literal
            return TextValue(data: token.value)
        case .QuotedName: // '...' // an explicitly delimited name literal
            // if next significant token is another expr, treat it as an RH operand (i.e. argument) and emit CommandValue, else return NameValue
            return try self.parseArgument(NameValue(data: token.value)) // speculatively parses for a command argument after the name; if none is found, backtracks to the name token and returns it as-is
            // VOCABULARY TOKENS
        case .NumericWord:
            return TextValue(data: token.value) // TO DO: add type info
        case .UnquotedName:
            return try self.parseArgument(NameValue(data: token.value)) // speculatively parses for a command argument after the name; if none is found, backtracks to the name token and returns it as-is
        case .Operator:
            print("parseAtom got Operator @\(self.lexer.currentTokenIndex): \(token.prefixOperator) \(token.infixOperator)")
            guard let operatorDefinition = token.prefixOperator else {
                self.lexer.backtrackTo(previousIndex)
                throw LeftOperandNotFoundError(description: "parseAtom() encountered an infix operator: \(token.infixOperator!)") // note: when speculatively parsing a command argument, this indicates the command has no argument so will be used as leftExpr; elsewhere it's a syntax error
            }
            let result = try operatorDefinition.parseFunc(self, leftExpr: nil, operatorName: operatorDefinition.name.name, precedence: precedence)
            print("... and returned it: \(result)")
            return result
            // UNSUPPORTED TOKENS (these cannot appear where an atom or prefix operator is expected)
        default:
            // Lexer should always convert .UnquotedWord tokens to one of the above vocabulary tokens; if parser encounters an .UnquotedWord token, that's a bug
            // AnnotationLiteralEnd, RecordLiteralEnd, ListLiteralEnd, ExpressionGroupLiteralEnd will only appear if opening '«', '{', '[', or '(' token is missing, in which case parser should treat as syntax error [current behavior] or as an incomplete structure in chunk of code whose remainder is in a previous chunk [depending how incremental parsing is done]; any other token types are infix/postfix ops which are missing their left operand
            return nil
        }
    }
    
    private func parseOperation(var leftExpr: Value, precedence: Int = 0) throws -> Value { // parse infix/postfix
        if DEBUG {print("\n\nENTERED parseOperation on \(self.lexer.currentTokenIndex), leftExpr=(\(leftExpr), \(precedence)) \n\t\ttoken=\(self.currentToken)\n\t\tnext=\(self.lexer.lookaheadBy(1))\n\n")}
        while precedence < self.precedenceForNextToken {
            let previousIndex = self.lexer.currentTokenIndex
            self.lexer.advance()
            if DEBUG {print("\nPARSE_OPER: advanced from \(previousIndex) to \(self.lexer.currentTokenIndex)\n\t\ttoken=\(self.lexer.currentToken)")}
            guard let token = self.lexer.currentToken else { throw EndOfCodeError(description: "[2] End of code.") } // outta tokens
            switch token.type {
                // PUNCTUATION TOKENS
            case .AnnotationLiteral: // «...» // attaches arbitrary contents to preceding node as metadata
                leftExpr.annotations.append(token.value)
            case .ExpressionSeparator: // period separator // note: comma and period seps are currently treated as postfix [no-]ops to preserve them // TO DO: this allows for somewhat silly inputs, e.g. `foo..,...,,,bar`, that should arguably be cleaned up or rejected
                return ExpressionSeparator(data: leftExpr) // as with linebreak token, period token (`.`) indicates end of expression (e.g. `1. - 2` is two expressions, not an operation), so is a no-op // TO DO: is it safe to return leftExpr without preserving period? any use cases where that would cause problems?
            case .ItemSeparator: // comma separator is normally used to separate list and record items; here it acts as an expression separator, though period separators are strongly preferred to avoid confusion
                return ItemSeparator(data: leftExpr) // ditto
            case .PairSeparator: // colon pair
                leftExpr = PairValue(name: leftExpr, data: try self.parseExpression(TokenType.PairSeparator.precedence-1))
            case .PipeSeparator: // semicolon pair
                // TO DO: would it be better to append RH expr to LH expr? (i.e. evaling LH expr would cause it to eval remaining piped commands as well, returning final result) or is it simplest just to have an object that specifically does this? (one issue with chaining [command] values is whether to go left-to-right or right-to-left; not that PipeValue exactly solves that)
                leftExpr = PipeValue(leftExpr: leftExpr, rightExpr: try self.parseExpression(TokenType.PipeSeparator.precedence))
            case .LineBreak: // linebreaks act as expression separators
                return leftExpr // TO DO: need to check this is correct // TO DO: check behavior is appropriate when linebreak appears between an operator and its operand[s]
                // VOCABULARY TOKENS
            case .Operator:
                print("parseOperation got Operator: \(token.prefixOperator) \(token.infixOperator)")
                guard let operatorDefinition = token.infixOperator else { // found a prefix operator, so end this expression and process it on next pass
                    self.lexer.backtrackTo(previousIndex)
                    return leftExpr
                }
                leftExpr = try operatorDefinition.parseFunc(self, leftExpr: leftExpr, operatorName:
                                                            operatorDefinition.name.name, precedence: operatorDefinition.precedence)
                // UNSUPPORTED TOKENS (these cannot appear where an infix/postfix operator is expected)
            default: // anything else implictly starts a new expression (if token is invalid, e.g. unbalanced bracket/brace/paren, if will be detected and thrown by parseAtom on next pass)
                self.lexer.backtrackTo(previousIndex)
                return leftExpr
            }
        }
        return leftExpr
    }
    
    // TO DO: should parseExpression be public? what use-cases does it offer vs parse()?
    
    func parseExpression(precedence: Int = 0) throws -> Value { // parse atom or prefix op, followed by any infix/postfix ops
        if DEBUG {print("\n\n[START] parseExpression: #\(self.lexer.currentTokenIndex) \(self.lexer.lookaheadBy(1))")}
        guard let leftExpr = try self.parseAtom(precedence) else {
            throw SyntaxError(description: "[1] Unexpected \"\(self.lexer.currentToken ?? nil)\"")
        }
        if DEBUG {print("[NEXT]  parseExpression: \(leftExpr)\n")}
        let result = try self.parseOperation(leftExpr, precedence: precedence)
        if DEBUG {print("[DONE]  parseExpression: \(result)\n")}
        return result
    }
    
    
    /**********************************************************************/
    // PARSE SCRIPT
    
    
    func parse() throws -> EntoliScript { // parse full script
        var result = [Value]()
     //   print("TOP-LEVEL parse() firstToken=", self.lexer.lookaheadBy(1))
        while self.lexer.lookaheadBy(1) != nil {
            result.append(self.stripPeriod(try self.parseExpression()))
            if DEBUG {print("TOP-LEVEL parse() completed expr: \(result.last)")}
            self.lexer.flush()
            
            break // DEBUG; delete
            
        }
        return EntoliScript(data: result)
    }
*/
}

