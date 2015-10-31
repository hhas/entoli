//
//  parser.swift
//  entoli
//
//

// TO KEEP SYNTAX RULES SIMPLE: numbers and units must start with optional `-`/`+` prefix followed by one of `0123456789`, and cannot contain whitespace, and they ALWAYS self-delimit.
// (this leaves currency to be decided, probably by allowing currency symbols as additional prefixes)

// TO DO: any reason why DDs can't apply themselves as symbol operator parsefuncs?

// Q. what is relationship between type specifiers (run-time coercions, specifically those that coerce from text to Swift primitives) and data detectors (parse-time readers)? bear in mind that if a DD can read a string, it can read either code or Text, so there's probably a lot of commonality in their core behavior

// Q. are there any use-cases where DDs wouldn't start with number (specifically, one of `-+0-9`)? (e.g. 42, 3.5kg, -123e4, +0.9, 0x1A2, 0u000A, 2015-06-12, 12:22:45 +0100)? Currency ($9.99) is one, any others?




// note: there's probably not much point worrying about preserving original whitespace vs automatically cleaning it up until code is sufficiently complete to allow testing; for now, just ignore issue entirely (it'll simplify implementation since e.g. parseName won't have to preserve original code substrings as well as generate canonical names - plus bear in mind that capturing code ranges in Values is useless in practice as any code edit invalidates all subsequent ranges)

// next problem: using recursion likely makes incremental parsing problematic

// TO DO: when parsing a record, format is arguably context-sensitive in that name:value pairs are evaled differently in proc signature definition vs command arg; suspect it might be best for users if all records require pair items to be 1. literal , and 2. `NameLiteral:any`; oh, and 3. this is required anyway in order to allow name to be anything, i.e. it is _always_ parsed as a NameValue, never a command or operator, e.g. `{to:foo}` should be a valid record, even though `to` is parsed as an operator name elsewhere


// note: another possibility for matching [e.g.] `-` is to rely on overloading, and have record arg with explicit keys (this'd avoid defining disambiguated names, e.g. `neg`, that aren't already visible, though arguably is a misuse of overloading in that behaviors are not simply variations on each other but quite different, e.g. negation vs subtraction)

// (note: currently, there aren't any keyword ops defined as atoms; the only reason to do so would be if defining a word or phrase that must always self-delimit itself, no matter where it appears, otherwise an arg-less command should be sufficient)






/*
// TO DO: when parsing param record, `to` operator should check result itself (this avoids need for parser to change behavior)

    func parseProcedureDefinition() -> Value { // TO DO: what return type? should be command
        switch token.type {
        case .QuotedName: // '...' // an explicitly delimited name literal
            // if next significant token is another expr, treat it as an RH operand (i.e. argument) and emit CommandValue, else return NameValue
            return try parseArgument(NameValue(data: token.value))
        case .UnquotedWord: // everything else that is not one of the above hardcoded token types; parseWord will deal with this
            return try parseArgument(try self.parseUnquotedWord(), precedence: precedence)
        default:
            throw SyntaxError("Malformed command signature.")
        }
        if self.lookahead(1) == Lexer.TokenType.RecordLiteral {
            arg = self.parseAtom()
            // need to check record is correctly structured: each item must be a name, optionally wrapped in coercion (don't forget optional annotations too - these will be treated as arg descriptions) // TO DO: is this right/optimal?... actually, no, it's the command's record that needs to be checked, as if it has pairs then LH must be name
        }

    }
*/


class SyntaxError: ErrorType, CustomStringConvertible {

    let description: String
    
    init(description: String = "Syntax error.") {
        self.description = description
    }

} // TO DO: how to store error info? (ideally, should be NSError-compatible without being dependent on it)

class EndOfCodeError: SyntaxError {}


/**********************************************************************/


class Parser {
    // TO DO: currently whitespace tokens are always included in lexer stream; if they usually aren't needed (e.g. to disambiguate adjoining tokens) then lexer could be revised to emit significant tokens only (alternatively, if lookahead and backtracking moves to lexer, could use 'includeWhiteSpace' arg in Lexer.lookahead() to check for whitespace separator only when it needs them, e.g. when parsing multi-word unquoted names)
    
    let lexer: Lexer
    var token: Lexer.Token? { return self.currentTokens[self.currentTokenIndex] } // current token
    
    // caches
    
    private var currentTokens: [Lexer.Token?] = [nil] // TO DO: need to watch when flushing cache in parse(): only delete up to currentTokenIndex, in case lookahead has already pulled tokens for next expression
    private var currentTokenIndex = 0
    
//    private var previousTokens: [Lexer.Token] = [] // TO DO: use single array [or linked list?] for backtracking and lookahead (note: would be cleaner in lexer, allowing it to be fully tested)
//    private var nextTokens: [Lexer.Token] = [] // used by lookahead() to cache tokens that have yet to be parsed
    
    
    let keywordOperators = StandardKeywordOperatorTable // TO DO: how to parameterize this?
    
    
    init(lexer: Lexer) {
        self.lexer = lexer
    }
    
    private func next(ignoreWhiteSpace: Bool) {
        repeat {
            self.currentTokenIndex += 1
            if self.currentTokenIndex == self.currentTokens.count { self.currentTokens.append(self.lexer.next()) }
        } while ignoreWhiteSpace && self.token?.type == .WhiteSpace
    }
    
    private func previous() { // TO DO: do we need to worry about whitespace at all? (probably not)
        self.currentTokenIndex -= 1
    }
    
    private func skip(tokenType: Lexer.TokenType, ignoreWhiteSpace: Bool = true) throws { // advance to next token, checking it's the required type and throwing SyntaxError if not
        self.next(ignoreWhiteSpace)
        if self.token?.type != tokenType {
            throw SyntaxError(description: "[0] Expected \(tokenType) but found \(self.token?.type)")
        }
    }
    
    
    private func backtrackTo(tokenIndex: Int) { // TO DO: need to check that backtracking always lands us on correct token; i.e. when breaking early to return leftExpr before starting on newExpr, need to backtrack by 1 extra so that next pass puts cursor back on that token
        self.currentTokenIndex = tokenIndex
    }
    
    // TO DO: would it be tidier to put lookahead (and backtracking?) support in Lexer?
    // caution: lookahead doesn't know about operators/data detectors; it can only look for punctuation, whitespace, quoted name/text, and unquoted word
    private func lookaheadBy(offset: UInt, ignoreWhiteSpace: Bool = true) -> Lexer.Token? { // TO DO: what about annotations? (should prob. also ignore those by default, but need to confirm)
        if offset == 0 { return self.token }
        var count: UInt = 0
        var lookaheadTokenIndex: Int = self.currentTokenIndex
        while count < offset {
            lookaheadTokenIndex += 1
            while lookaheadTokenIndex >= self.currentTokens.count { self.currentTokens.append(self.lexer.next()) }
            guard let lookaheadToken = self.currentTokens[lookaheadTokenIndex] else { return nil }
            if lookaheadToken.type != .WhiteSpace || !ignoreWhiteSpace { count += 1 }
        }
        return self.currentTokens[lookaheadTokenIndex]
    }
    
    // utility funcs; used to clean up collection items
    
    private func stripComma(value: Value) -> Value { // used to tidy list items after parsing; TO DO: should this strip _all_ commas? or treat adjoining commas as syntax error/warning (e.g. [1,2,,4] is legal but suggests a typo)? or just leave as-is and leave user to clean up surplus commas in code if it bothers them
        if let v = value as? ItemSeparator { return v.data }
        return value
    }
    
    private func stripPeriod(value: Value) -> Value { // TO DO: ditto
        if let v = value as? ExpressionSeparator { return v.data }
        return value
    }
    
    //
    
    private struct Precedence { // standard punctuation tokens // TO DO: put this and switch on TokenType enum?
        static let AnnotationLiteral = 1000
        static let ExpressionSeparator = 50
        static let ItemSeparator       = 50
        static let PairSeparator       = 60
        static let PipeSeparator       = 50
    }
    
    private func precedence(lookahead: UInt) -> Int { // TO DO: this gets a bit thorny with non-breaking whitespace tokens (Q. what about linebreaks/comments/annotations?); lookahead should always skip those // TO DO: what to return on EOF (currently -1; nil? Error?)
        guard let token = lookahead == 0 ? self.token : self.lookaheadBy(lookahead) else { return -1 }
        // TO DO: check for table-defined operator precedence before looking up built-ins (Q. how? this can only check a word; we need to parseUnquotedWord to determine what a word sequence actually represents, and what, if any, operators it contains - one option is to parse the unquoted word to yield new set of pseudo-tokens which carry their own precedence, and thus can be checked; these would, presumably be Atom/PrefixOp/InfixOp/PostfixOp enumerations, and would be parameterized with already-parsed values and op-info)
        switch token.type { 
        case .AnnotationLiteral:    return Precedence.AnnotationLiteral
        case .ExpressionSeparator:  return Precedence.ExpressionSeparator
        case .ItemSeparator:        return Precedence.ItemSeparator
        case .PairSeparator:        return Precedence.PairSeparator
        case .PipeSeparator:        return Precedence.PipeSeparator
        default:                    return 0
        }
    }
    
    
    /**********************************************************************/
    // PARSE COMMAND ARGUMENT
    
    private func parseArgument(name: NameValue, precedence: Int = 0) throws -> Value {
        // called by parseAtom after parsing a quoted/unquoted name
        // given a NameValue, return a CommandValue if it's followed by a valid argument, otherwise return NameValue unchanged
        // note: command name and argument will always bind more tightly to each other than to operators
        // note: this is right-associative, so `'foo' 'bar' 'baz'` will parse as `foo {bar {baz}}`
        let backtrackIndex = self.currentTokenIndex
        var argument: Value?
        do {
            argument = try self.parseAtom() // this'll throw EOF error if command name is last
        } catch is EndOfCodeError {
            argument = nil
        }
        if argument == nil { // name is not followed by a new expression, so it is either just a name or an argument-less command; in either case, return it unchanged as NameValue
            self.backtrackTo(backtrackIndex)
//            print("parseArgument backtracked to \(self.token)")
            return name // TO DO: if next token is `do...done` block (a postfix structure), confirm that this gets attached to command correctly (note that its parsefunc will need to cast name to command first)
        } else {
            if !(argument is RecordValue) { argument = RecordValue(data: [argument!]) } // non-record values are treated as first item in a record arg
            // any pairs in argument record *must* have NameValue as LH operand (in theory, they could be treated as positional if LH is non-name, but this will likely cause user confusion), so check and throw syntax error if not (e.g. to pass a pair as a positional arg, just wrap in parens, though bear in mind how it's evaled will depend on context)
            var i = 1
            for item in (argument as! RecordValue).data {
                if item is PairValue && !((item as! PairValue).name is NameValue) {
                    throw SyntaxError(description: "Malformed record argument for \(name) command: item \(i) is a name-value pair, but its left side is not a literal name: \(item)")
                }
                i += 1
            }
            return CommandValue(name: name, data: argument!) // TO DO: if next token is a postfix `do...done` block, confirm that this gets attached to command correctly (i.e. its parsefunc should coerce its LH operand to Command, then add parsed block to it)
        }
    }
    
    
    /**********************************************************************/
    // PARSE EXPRESSION
    // TO DO: parseExpression() is currently split into two sub-functions: parseAtom() and parseOperation(); currently, parseArgument() calls parseAtom() directly, so implicitly always binds more tightly to its argument than the argument binds to any subsequent operator; need to decide if this is appropriate (as opposed to parseArgument calling parseExpression with very high precedence). Mostly it's about determining if a name expr is followed by a new expr (since commands don't have an explicit infix operator symbol to join name and arg, we have to do special-case pattern matching): if it does (which parseAtom can tell us: any non-nil result means a new expr was found), then it's a name-arg pair, i.e. a command with a single arg. Main question then is whether we should pass that arg to parseOperation along with the command's [high] precedence, in case there are any operators that might justfiably want to bind even tighter to that arg than the command does. For now though, all infix/postfix ops following the command (name-arg pair) will receive the entire command as their LH operand. (Plus, don't forget commands are implicitly right-associative, so `'foo' 'bar' 'baz' and 'fub'` will parse as `and{foo{bar{baz}},fub}`).
    
    private func parseAtom(precedence: Int = 0) throws -> Value? { // parse atom or prefix op (i.e. no left operand)
        self.next(true)
//        print("\nPARSE_PREFIX: \(self.token)")
        guard let token = self.token else { throw EndOfCodeError(description: "[1] End of code.") } // outta tokens
        switch token.type {
        case .AnnotationLiteral: // «...» // attaches arbitrary contents to subsequent node as metadata
            let annotation = token.value
            let value = try self.parseExpression(Precedence.AnnotationLiteral) // bind like glue
            value.annotations.append(annotation)
            return value
            // note: rest of these are atoms (no ops/precedence)
        case .ListLiteral: // [...];  an ordered collection (array) or key-value collection (dictionary)
            var result = [Value]()
            // TO DO: if all items are pairs, make this an associative list (note: need to decide rules by which ordered lists [Array], associative lists [Dict], and unique lists [Set] are tagged and/or coercion-locked as that type; this will largely be determined by usage, e.g. once an array operation is performed, the list should thereafter act as array and refuse to coerce to dict or set - though explicit casting should still be allowed; of course, a lot depends on whether lists are mutable or immutable, and that policy/mechanism has yet to be decided)
            while self.lookaheadBy(1)?.type != .ListLiteralEnd {
                let value = self.stripComma(try self.parseExpression()) // parser treats comma separator as postfix op to preserve it, but collections are self-delimiting so don't need it
                result.append(value)
            }
            try self.skip(.ListLiteralEnd) // TO DO: skip is only really needed to throw error if code terminates without closing `]`
            return ListValue(data: result)
        case .RecordLiteral: // {...}; a sequence of values and/or name-value pairs; mostly used to pass proc args
            //       print("")
            var result = [Value]()
            while self.lookaheadBy(1)?.type != .RecordLiteralEnd {
           //     guard let nextTokenType = self.lookaheadBy(1)?.type else { throw EndOfCodeError(description: "[1a] End of code.") }
                guard let nextToken = self.lookaheadBy(1) else { throw EndOfCodeError(description: "[1a] End of code.") }
                let isLegalPair: Bool
                let value: Value
                switch nextToken.type { // note: if item looks like a valid `name:value pair`, need to check pairs' precedence to make sure infix/postfix operators never bind entire pair where they're really intended to bind RH operand only (if they do bind entire pair, it shouldn't break following logic, but it will confuse users since what looks like a named arg is actually a positional one; possibly the safest option is for all operators to have higher precedence than punctuation, and enforce this in OperatorTable; though we will need to watch for 'stray' punctuation getting sucked up in LH operand when it's supposed to delimit it)
                case .QuotedName: // check for `'NAME' :`
                    isLegalPair = self.lookaheadBy(2)?.type == .PairSeparator // literal pairs in records MUST have literal name as LH operand (note: this looks ahead 2, since currentToken is `{` and next token is QuotedName)
                    value = self.stripComma(try self.parseExpression())
                case .UnquotedWord: // check for [e.g.] `WORD WORD WORD :`
                    let backtrackIndex = self.currentTokenIndex
                    var words = [String]()
                    repeat {
                        self.next(true)
                        words.append(self.token!.value)
                    } while self.hasNextWord
                    isLegalPair = self.lookaheadBy(1)?.type == .PairSeparator
                    if isLegalPair { // it's a pair with only words on left side, so treat those words as pair's name (i.e. no data detection or operator name matching is performed; even numbers are legal), and parse the rest of the pair
                        value = self.stripComma(try self.parseOperation(NameValue(data: words.joinWithSeparator(" "))))
                    } else {
                        self.backtrackTo(backtrackIndex)
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
                //            print("CURR: \(self.token)\n")
            }
            try self.skip(.RecordLiteralEnd) // TO DO: skip is only really needed to throw error if code terminates without closing `]`
            return RecordValue(data: result)
        case .ExpressionGroupLiteral: // (...)
            var result = [Value]()
            while self.lookaheadBy(1)?.type != .ExpressionGroupLiteralEnd {
                let value = self.stripPeriod(try self.parseExpression())
                result.append(value)
            }
            try self.skip(.ExpressionGroupLiteralEnd)
            return ExpressionGroupValue(data: result)
        case .LineBreak: // linebreaks act as name (and expr?) separators
            return try self.parseExpression() // skip linebreak and parse next line as new expression // TO DO: prob. should preserve linebreak (e.g. as postfix no-op) for display purposes (i.e. reformatting user's code is one thing; reformatting their layout is another)
            // atomic literals
        case .QuotedText: // "..." // a text (string) literal
            return TextValue(data: token.value)
        case .QuotedName: // '...' // an explicitly delimited name literal
            // if next significant token is another expr, treat it as an RH operand (i.e. argument) and emit CommandValue, else return NameValue
            return try parseArgument(NameValue(data: token.value))
        case .UnquotedWord: // everything else that is not one of the above hardcoded token types; parseWord will deal with this
            // TO DO: confirm this never needs to backtrack, i.e. always returns Name/Command or error, and does any backtracking itself
            let result = try self.parseUnquotedWord() // may be NameValue (if it's a simple sequence of unquoted words with no special meaning), TextValue (if a data detector matched it), or CommandValue (if some or all of it was matched against operators table)
 //           print("parseAtom.UnquotedWord got Name/Command \(result)...")
            let res = result is NameValue ? try parseArgument(result as! NameValue, precedence: precedence) : result // TO DO: actually need to check why precedence is passed here, and what affect it has (it _should_ only be used to prevent overshoot _if_ parseArgument also calls parseOperation [which it currently doesn't do, ensuring arg always binds tightest to command], so is currently unused here, and if parseArgument doesn't change then it probably makes most sense to take it out)
//            print("... and returned it as \(res)")
            return res
        default:
            // TO DO: AnnotationLiteralEnd, RecordLiteralEnd, ListLiteralEnd, ExpressionGroupLiteralEnd will only appear if opening '«', '{', '[', or '(' token is missing, in which case parser should treat as syntax error [current behavior] or as an incomplete structure in chunk of code whose remainder is in a previous chunk [depending how incremental parsing is done]; any other token types are infix/postfix ops which are missing their left operand
            return nil
        }
    }
    
    private func parseOperation(var leftExpr: Value, precedence: Int = 0) throws -> Value { // parse infix/postfix
        while precedence < self.precedence(1) || self.lookaheadBy(1)?.type == .UnquotedWord { // TO DO: precedence(1) will return 0 if next token is unquoted word, but until word seq has been parsed we can't know its actual precedence
//                        print("\nPARSE_INFIX:  \(self.token)")
            let startID = self.currentTokenIndex
            self.next(true)
            guard let token = self.token else { throw EndOfCodeError(description: "[2] End of code.") } // outta tokens
            switch token.type {
            case .AnnotationLiteral: // «...» // attaches arbitrary contents to preceding node as metadata
                let annotation = token.value
                leftExpr.annotations.append(annotation)
            case .ExpressionSeparator: // period // note: comma and period seps are currently treated as postfix [no-]ops to preserve them // TO DO: this allows for somewhat silly inputs, e.g. `foo..,...,,,bar`, that should arguably be cleaned up or rejected
                return ExpressionSeparator(data: leftExpr) // this should be a no-op // TO DO: pretty certain this should return, as `.` indicates end of expression (e.g. `1. - 2` is two exprs, not a sub op); confirm this
            case .ItemSeparator: // comma
                return ItemSeparator(data: leftExpr) // ditto
            case .PairSeparator: // colon
                leftExpr = PairValue(name: leftExpr, data: try self.parseExpression(Precedence.PairSeparator-1))
            case .PipeSeparator: // semicolon
                // TO DO: would it be better to append RH expr to LH expr? (i.e. evaling LH expr would cause it to eval remaining piped commands as well, returning final result) or is it simplest just to have an object that specifically does this? (one issue with chaining [command] values is whether to go left-to-right or right-to-left; not that PipeValue exactly solves that)
                leftExpr = PipeValue(leftExpr: leftExpr, rightExpr: try self.parseExpression(Precedence.PipeSeparator))
            case .LineBreak: // linebreaks act as name (and expr?) separators
                return leftExpr // TO DO: need to check this is correct // TO DO: check behavior is appopriate when linebreak appears between an operator and its operand[s]
            case .UnquotedWord:
                // result is either leftExpr (i.e. word is start of a new expression), or an infix/postfix operator with leftExpr as its LH operand
                let result = try self.parseUnquotedWord(leftExpr, precedence: precedence, backtrackIndex: startID) // TO DO: return value is dependent on whether unquoted word(s) are infix/postfix operators acting on leftExpr, or new atom/prefix operator that delimits self from leftExpr
                if result === leftExpr { // parseQuotedWord detected start of new expression, or doesn't bind tightly enough to leftExpr, so return leftExpr and process UnquotedWord token on next pass
                    self.previous() // the current token is the first unquoted word, so need to move back one so that next pass will advance back onto it
                    break
                }
                leftExpr = result
            default:
                throw SyntaxError(description: "[2] Unexpected \"\(token)\"")
            }
        }
        return leftExpr
    }
    
    func parseExpression(precedence: Int = 0) throws -> Value { // parse atom or prefix op, followed by any infix/postfix ops
  //      print("\nSTART parseExpression: #\(self.currentTokenIndex) \(self.token)")
        guard let leftExpr = try self.parseAtom(precedence) else {
            throw SyntaxError(description: "[1] Unexpected \"\(self.token ?? nil)\"")
        }
        return try self.parseOperation(leftExpr, precedence: precedence)
    }
    
    
    /**********************************************************************/
    // PARSE WORDS
    
    typealias PartialKeywordOperatorMatch = (words: [String], startID: Int, matchInfo: KeywordOperatorTable.WordInfoType)
    typealias FullKeywordOperatorMatch = (words: [String], startID: Int, endID: Int, operatorDefinition: OperatorDefinition)
    
    private var hasNextWord: Bool { return self.lookaheadBy(1, ignoreWhiteSpace: false)?.type == Lexer.TokenType.WhiteSpace
                                                                 && self.lookaheadBy(1)?.type == Lexer.TokenType.UnquotedWord }
    
    private func parseUnquotedWord(var leftExpr: Value? = nil, precedence: Int = 0, backtrackIndex: Int? = nil) throws -> Value { // called by parseAtom(), parseOperator() for current (non-nil) token
        // TO DO: this currently parses sequence of words and returns canonical name, but needs to apply operators and data detectors as well; in addition, also needs to return CommandValue if name is followed by another expression (i.e. arg) - although that might be better done from parseExpr using precedence [hmmm... not sure about that, usually it's parsefuncs that continue consuming till they've made their match, though here we would need to match RH to determine if it's a value (including another command)/separator (in which case return)/operator (in which case this word is either its LH operand, in which case proceed to build operator, or - if op is prefix - then this is implicitly end of expr)]; given that pairs require different parsing of LH operand depending on context (struct or code vs list, might be good to have separate `parseName` and `parseKeywordExpr` methods)
 //       print("    ParseUnquotedWord: leftExpr=\(leftExpr) token=\(self.token?.type) \(self.token?.value)")
        var words = [String]()
        // match keyword operators
        var partialKeywordOperatorMatches = [PartialKeywordOperatorMatch]()
        var fullKeywordOperatorMatches = [FullKeywordOperatorMatch]()
        let firstWordID = self.currentTokenIndex
        // scan all words until the first full [and longest] operator match is made (caveat: if it's not left and right auto-delimiting, need to discard if there are adjoining words)
        while self.token != nil { // TO DO: as soon as that match is made, loop should exit (no point reading any further as without caching it'll all get tossed anyway, plus parsefuncs have their own ideas about what to look for, especially when reading more complex structures such as `do...done` blocks)
            words.append(self.token!.value) // note: words always contains current token, including first word of a keyword op (this will be removed again before constructing the left NameValue to an operator)
            var i = 0
            for (precedingWords, startID, match) in partialKeywordOperatorMatches {
                if let operatorDefinition = match.definition {
                    // note: if the matched "operator" is NOT left-self-delimiting, it must be first in word sequence; conversely, if it is NOT right-self-delimiting, it must be last. If these conditions are not met then it is not an operator, just normal word[s] within a longer phrase. e.g. The `to` prefix operator is not left-self-delimiting, so `to foo` is a valid `to` op with 'foo' as its RH operand, but `go to` is just an ordinary name (i.e. the 'to' is not special as it is not the first word in the expression).
                    if (operatorDefinition.autoDelimit.left || startID == firstWordID) && (operatorDefinition.autoDelimit.right || !self.hasNextWord) {
//                        print(" FOUND A FULL MATCH: \((precedingWords, startID, self.currentTokenIndex, operatorDefinition))")
                        fullKeywordOperatorMatches.append((precedingWords, startID, self.currentTokenIndex-1, operatorDefinition)) // note: currentTokenIndex is next word, so nudge back one so that next pass puts us back on it
                    }
                }
                if let nextMatch = match.nextWords[self.token!.value] {
                    partialKeywordOperatorMatches[i] = (precedingWords, startID, nextMatch)
                    i += 1
                } else {
                    partialKeywordOperatorMatches.removeAtIndex(i)
                }
            }
            for definitions in [self.keywordOperators.prefixDefinitions, self.keywordOperators.infixDefinitions] {
                if let foundOp = definitions[self.token!.value] {
                    partialKeywordOperatorMatches.append((words, self.currentTokenIndex, foundOp))
                }
            }
            if !self.hasNextWord { break }
            self.next(true)
        }
        if fullKeywordOperatorMatches.count > 0 {
            fullKeywordOperatorMatches.sortInPlace{ ($0.startID < $1.startID) || ($0.startID == $1.startID && $0.endID > $1.endID) }
            let foundOp = fullKeywordOperatorMatches[0]
 //           print("\nCURRENT TOKEN: \(self.token)")
            self.backtrackTo(foundOp.endID)
//            print("...IDENTIFIED BEST-MATCH OPERATOR: \(foundOp); \n\tbacktrack to \(self.token)")
            let operatorDefinition = foundOp.operatorDefinition
            if foundOp.startID == firstWordID {
                //print("MATCHED OP AT START OF WORDS; \(leftExpr) \(operatorDefinition.form) \(operatorDefinition.form.hasLeftOperand)")
                if operatorDefinition.form.hasLeftOperand { // it's an infix/postfix op (i.e. leftExpr is required) so do checks before proceeding
                    if leftExpr == nil {
                        throw SyntaxError(description: "'" + words.joinWithSeparator(" ") + "' operator requires left-hand operand.")
                    } else if !(precedence < operatorDefinition.precedence) { // TO DO: need to confirm this is correct
                        return leftExpr!
                    }
                } else if leftExpr != nil { // it's an atom/prefix op, so make sure that it's start of a new expression (i.e. no leftExpr is given)
                    return leftExpr! // leftExpr was followed by new expr, new expr will be parsed on next pass
                }
            } else { // we found one or more non-operator words (i.e. a name), followed by some sort of operator
                if leftExpr != nil { return leftExpr! } // if leftExpr was given, we have to return that first before we can process this new expression; new expr will be parsed on next pass
                leftExpr = NameValue(data: foundOp.words[0..<(foundOp.words.count-1)].joinWithSeparator(" "))
                // Q. how to we determine word range? (easiest is to copy words to partial)
            }
            // cursor is positioned to start reading RH operand (if any), so call the operator's parsefunc to read that and return the finished command expression
            // caution: leftExpr is ImplicitlyUnwrappedOptional<Value>, so never pass nil to infix/postfix parsfuncs or they'll crash
            return try operatorDefinition.parseFunc(self, leftExpr: leftExpr, operatorName: operatorDefinition.name, precedence: operatorDefinition.precedence)
        } else {
            return NameValue(data: words.joinWithSeparator(" ")) // no operators were found, so return entire phrase as NameValue (caller will decide what to do with it, e.g. check for a RH argument and convert to CommandValue if one is found)
        }
    }
    
    /**********************************************************************/
    // PARSE SCRIPT
    
    
    func parse() throws -> EntoliScript { // parse full script
        var result = [Value]()
        while self.lookaheadBy(1) != nil {
            result.append(try self.parseExpression())
            self.currentTokens.removeRange(0..<self.currentTokenIndex) // clear cache of fully-parsed tokens
            self.currentTokenIndex = 0
        }
        return EntoliScript(data: result)
    }
}


