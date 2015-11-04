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


/**********************************************************************/


class Parser {
    // TO DO: currently whitespace tokens are always included in lexer stream; if they usually aren't needed (e.g. to disambiguate adjoining tokens) then lexer could be revised to emit significant tokens only (alternatively, if lookahead and backtracking moves to lexer, could use 'includeWhiteSpace' arg in VocabularyLexer.lookahead() to check for whitespace separator only when it needs them, e.g. when parsing multi-word unquoted names)
    
    let lexer: Lexer
    
    init(lexer: Lexer) {
        self.lexer = lexer
    }
    
    
    /**********************************************************************/
    // TOKENS
    
    // TO DO: need to maintain cache of punctuation+vocabulary tokens
    
    var currentToken: Token? { return self.lexer.currentToken } // current token
    
    private var precedenceForNextToken: Int { // TO DO: this gets a bit thorny with non-breaking whitespace tokens (that said, linebreaks should always delimit exprs; the only ones to worry about are in groups) (Q. what about linebreaks/comments/annotations?); lookahead should always skip those // TO DO: what to return on EOF (currently -1; nil? Error?)
        guard let token = self.lexer.lookaheadBy(1) else { return -1 } // TO DO: this isn't reliable if vocab lexer has moved punc lexer on further; nor for that matter is it reliable (since tokens take on different meaning while parsing vocab, e.g. periods as decimal separator)
        
        print("precedenceForNextToken: \(token)")
        
        return 0 // TO DO: FIX!!!!!!!!
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
    
    private func parseArgument(name: NameValue, precedence: Int = 0) throws -> Value {
        // called by parseAtom after parsing a quoted/unquoted name
        // given a NameValue, return a CommandValue if it's followed by a valid argument, otherwise return NameValue unchanged
        // note: command name and argument will always bind more tightly to each other than to operators
        // note: this is right-associative, so `'foo' 'bar' 'baz'` will parse as `foo {bar {baz}}`
        let backtrackIndex = self.lexer.currentTokenIndex
        var argument: Value?
        do {
            argument = try self.parseAtom() // this'll throw EOF error if command name is last
        } catch is EndOfCodeError {
            argument = nil
        }
        if argument == nil { // name is not followed by a new expression, so it is either just a name or an argument-less command; in either case, return it unchanged as NameValue
            self.lexer.backtrackTo(backtrackIndex)
            //print("parseArgument backtracked to \(self.currentToken)")
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
    
    private func parseUnquotedWord(leftExpr: Value? = nil, precedence: Int = 0) throws -> Value? {
        
        print("parseUnquotedWord: \(leftExpr), \(precedence), <\(self.currentToken?.value)>")
        
 //       repeat {
//        self.vocabularyLexer.advance()
        
 //       print(self.vocabularyLexer.currentToken)
            
  //      self.lexer.advance() // parseAtom/parseOp would normally call this
            
 //       } while self.vocabularyLexer.currentToken != nil
        
        throw SyntaxError(description: "TODO")
        
        //
        
    }
    
    /*

/*
    private func verifyFullOperatorMatch(precedingWords: [String], startIndex: Int, endIndex: Int, ) throws -> Value? {
        
        // foundOp is prefix/infix, depending on what's available and which is more suitable (e.g. infix is preferred if something's on its left)
        
        //print("MATCHES:" + (fullOperatorPhraseMatches.map{"\($0)"}.joinWithSeparator("\n\t\t")))
        //print("\nCURRENT TOKEN: \(self.currentToken)")
        self.backtrackTo(foundOp.endIndex)
        //print("...IDENTIFIED BEST-MATCH OPERATOR: \(foundOp); \n\tbacktrack to \(self.currentToken)")
        let operatorDefinition = foundOp.operatorDefinition
        if foundOp.startIndex == firstWordIndex {
            //print("MATCHED OP AT START OF WORDS; \(leftExpr) \(operatorDefinition.form) \(operatorDefinition.form.hasLeftOperand)")
            if operatorDefinition.form.hasLeftOperand { // it's an infix/postfix op (i.e. leftExpr is required) so do checks before proceeding
                if leftExpr == nil { // can't have infix/postfix op without an LH operand, so throw SyntaxError
                    throw SyntaxError(description: "'" + foundOp.words.joinWithSeparator(" ") + "' operator requires left-hand operand.")
                } else if !(precedence < operatorDefinition.precedence) { // now we know the op, we can check whether leftExpr binds more tightly to this op than whatever preceded it; if not, return now, and the same test will be performed on all of that LH expr, and so on
                    return nil
                }
            } else { // it's an atom/prefix op, so make sure that it's start of a new expression (i.e. no leftExpr is given)
                if leftExpr != nil { return nil } // parseOperator needs to return leftExpr first; new expr will be parsed on next pass
            }
        } else { // it's a new expression consisting of one or more non-operator words (i.e. a name), followed by some sort of operator
            print("MATCHED OP IN WORDS; \(leftExpr)")
            if leftExpr != nil { return nil } // ditto
            leftExpr = NameValue(data: foundOp.words[0..<(foundOp.words.count-1)].joinWithSeparator(" "))
            //print("....... OP IN WORDS; \(leftExpr)")
            print("CHECK PREC: \(leftExpr) prevOp=\(precedence) op=\(operatorDefinition.precedence)")
            if !(precedence < operatorDefinition.precedence) { // now we know the op, we can check whether leftExpr binds more tightly to this op than whatever preceded it; if not, return now, and the same test will be performed on all of that LH expr, and so on
                self.backtrackTo(foundOp.startIndex)
                print("RETURN \(leftExpr) next=\(self.currentToken)")
                self.retreat()
                return leftExpr
            }
        }
        // sanity check
        if leftExpr != nil && !operatorDefinition.form.hasLeftOperand || leftExpr == nil && operatorDefinition.form.hasLeftOperand {
            throw SyntaxError(description: "PARSER BUG: mismatched operator: leftExpr=\(leftExpr) opDef=\(operatorDefinition)") // BUG
        }
        // lexer is now positioned to start reading RH operand (if any), so call the operator's parsefunc to read that (plus any additional operands if it's a multifix op, keyword-based block, etc.) and return the finished command expression
        // caution: parsefuncs treat leftExpr as an ImplicitlyUnwrappedOptional<Value>, so never pass nil to infix/postfix parsfuncs unless they're already expecting this (e.g. a parsefunc that handles both prefix and infix/postfix cases), or they'll crash if they try to use it
        return try operatorDefinition.parseFunc(self, leftExpr: leftExpr, operatorName: operatorDefinition.name.name, precedence: operatorDefinition.precedence)
    }

*/


    private func processOperatorPhrase(match: OperatorPhrasesTable.WordInfoType, isFirstWord: Bool, startIndex: Int, leftExpr: Value?) throws -> Value? {
        
        // note: we only need to collect partial matches until we have our first full match, and have verified isValidOperatorName; once we have that, we only need to scan for additional full matches deriving from that first match (e.g. given `A is not same as B`, once ` we match `is`, we keep going until we've got `is not`, `is not same as`, at which point we hit isLongest); as long as we verify isValidOperatorName each time and only add those that are to `fullMatches`, we're good. Once we hit isLongest, we use the last match in that list as our operator definition.
        // now find longest valid match
        var longestMatch = (endIndex: self.lexer.currentTokenIndex, match: match) // TO DO: include `literalOperatorNameEnd:currentToken.range.end` (caveat ±1), which can be combined with  // TO DO: does endIndex still need budged? note: lexer.currentTokenIndex is next word, so nudge endIndex back one so that next pass puts us back on it
        var match = match
        self.lexer.advance()
        while self.lexer.currentToken != nil && !longestMatch.match.isLongest {
            let token = self.lexer.currentToken!
            guard let nextMatch = match.nextWords[token.value] else { break } // no more matches to be made
            match = nextMatch
            if self.isValidOperatorName(match.prefixDefinition, isFirstWord: isFirstWord)
                    || self.isValidOperatorName(match.infixDefinition, isFirstWord: isFirstWord) { // found a longer match, so replace previous longest match
                longestMatch = (endIndex: self.lexer.currentTokenIndex, match: match)
            }
        }
        // now we've got the operator name, figure out its operands, and return
        
        // if leftExpr is nil and an infix/postfix op is found after first word, use its preceding word(s) as its LH operand
        // if leftExpr is Value, an infix/postfix op at start should be applied to that (precedence willing); anything else,  treat these words as new expr, so backtrack then return leftExpr
        
        
        
        // note, once we've got longest match, we should be able to use same verification for both symbol and keyword:
        /*if leftExpr == nil { // this is new expression
        if isFirstWord { // note: only atom/prefix ops are allowed at start of new expression
        operatorDefinition = match.prefixDefinition
        if operatorDefinition == nil { // must have matched an infix/postfix operator, which is not allowed at start of expression
        throw SyntaxError(description: "Infix/postfix operator is missing LH operand: \(match.prefixDefinition)")
        }
        } else { // use preceding word[s] as new leftExpr, and either return (if op is prefix) or use as LH operand (if op is infix)
        
        }
        } else { // leftExpr is given, so we either need to apply an infix op to that if (precedence < operatorDefinition.precedence), or return it first
        if isFirstWord { // note: infix/postfix ops should use some leftExpr as LH operand; anything else is new expression so must return leftExpr first
        } else { // preceding words are [command] name, followed by a prefix/infix op
        
        }
        }*/
        
        return nil
    }
    
    */
    
    /**********************************************************************/
    // PARSE EXPRESSION
    
    private func parseAtom(precedence: Int = 0) throws -> Value? { // parse atom or prefix op (i.e. no left operand)
        self.lexer.advance()
        if DEBUG {print("\nPARSE_PREFIX: \(self.lexer.currentToken)")}
        guard let token = self.lexer.currentToken else { throw EndOfCodeError(description: "[1] End of code.") } // outta tokens
        switch token.type {
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
            return try parseArgument(NameValue(data: token.value))
        case .UnquotedWord: // everything else that is not one of the above hardcoded token types; parseWord will deal with this
            // TO DO: confirm this never needs to backtrack, i.e. always returns Name/Command or error, and does any backtracking itself
            let result = try self.parseUnquotedWord() // may be NameValue (if it's a simple sequence of unquoted words with no special meaning), TextValue (if a data detector matched it), or CommandValue (if some or all of it was matched against operators table)
            //print("parseAtom.UnquotedWord got Name/Command \(result)...")
            let res = result is NameValue ? try self.parseArgument(result as! NameValue, precedence: precedence) : result // TO DO: actually need to check why precedence is passed here, and what affect it has (it _should_ only be used to prevent overshoot _if_ parseArgument also calls parseOperation [which it currently doesn't do, ensuring arg always binds tightest to command], so is currently unused here, and if parseArgument doesn't change then it probably makes most sense to take it out)
            //print("... and returned it as \(res)")
            //print("PARSED UQW: \(res)")
            return res // TO DO: what if parseUnquotedWord returns nil here for some reason; what will that do, and should it be guarded against as error?
        default:
            // TO DO: AnnotationLiteralEnd, RecordLiteralEnd, ListLiteralEnd, ExpressionGroupLiteralEnd will only appear if opening '«', '{', '[', or '(' token is missing, in which case parser should treat as syntax error [current behavior] or as an incomplete structure in chunk of code whose remainder is in a previous chunk [depending how incremental parsing is done]; any other token types are infix/postfix ops which are missing their left operand
            return nil
        }
    }
    
    private func parseOperation(var leftExpr: Value, precedence: Int = 0) throws -> Value { // parse infix/postfix
        if DEBUG {print("parseOperation(\(leftExpr), \(precedence)) \n\t\ttoken=\(self.currentToken)\n\t\tnext=\(self.lexer.lookaheadBy(1))")}
        while precedence < self.precedenceForNextToken {
            if DEBUG {print("\nPARSE_INFIX:  \(self.lexer.currentToken)")}
            let previousIndex = self.lexer.currentTokenIndex
            self.lexer.advance()
            guard let token = self.lexer.currentToken else { throw EndOfCodeError(description: "[2] End of code.") } // outta tokens
            switch token.type {
            case .AnnotationLiteral: // «...» // attaches arbitrary contents to preceding node as metadata
                let annotation = token.value
                leftExpr.annotations.append(annotation)
            case .ExpressionSeparator: // period // note: comma and period seps are currently treated as postfix [no-]ops to preserve them // TO DO: this allows for somewhat silly inputs, e.g. `foo..,...,,,bar`, that should arguably be cleaned up or rejected
                return ExpressionSeparator(data: leftExpr) // this should be a no-op // TO DO: pretty certain this should return, as `.` indicates end of expression (e.g. `1. - 2` is two exprs, not a sub op); confirm this
            case .ItemSeparator: // comma
                return ItemSeparator(data: leftExpr) // ditto
            case .PairSeparator: // colon
                leftExpr = PairValue(name: leftExpr, data: try self.parseExpression(TokenType.PairSeparator.precedence-1))
            case .PipeSeparator: // semicolon
                // TO DO: would it be better to append RH expr to LH expr? (i.e. evaling LH expr would cause it to eval remaining piped commands as well, returning final result) or is it simplest just to have an object that specifically does this? (one issue with chaining [command] values is whether to go left-to-right or right-to-left; not that PipeValue exactly solves that)
                leftExpr = PipeValue(leftExpr: leftExpr, rightExpr: try self.parseExpression(TokenType.PipeSeparator.precedence))
            case .LineBreak: // linebreaks act as name (and expr?) separators
                return leftExpr // TO DO: need to check this is correct // TO DO: check behavior is appopriate when linebreak appears between an operator and its operand[s]
            case .UnquotedWord:
                // parseUnquotedWord returns nil if word is start of a new expression, or an infix/postfix operator with leftExpr as its LH operand
                if let result = try self.parseUnquotedWord(leftExpr, precedence: precedence) {
                    leftExpr = result // TO DO: what about parseArgument? need to sort this crap out
                } else { // parseQuotedWord detected start of new expression (or doesn't bind tightly enough to leftExpr(?)), so return leftExpr as-is and process UnquotedWord token on next pass
                    self.lexer.backtrackTo(previousIndex)
                    return leftExpr
                }
            default:
                throw SyntaxError(description: "[2] Unexpected \"\(token)\"")
            }
        }
        return leftExpr
    }
    
    // TO DO: should parseExpression be public? what use-cases does it offer vs parse()?
    
    func parseExpression(precedence: Int = 0) throws -> Value { // parse atom or prefix op, followed by any infix/postfix ops
        if DEBUG {print("\nSTART parseExpression: #\(self.lexer.currentTokenIndex) \(self.lexer.lookaheadBy(1))")}
        guard let leftExpr = try self.parseAtom(precedence) else {
            throw SyntaxError(description: "[1] Unexpected \"\(self.lexer.currentToken ?? nil)\"")
        }
        let result = try self.parseOperation(leftExpr, precedence: precedence)
        if DEBUG {print("DONE  parseExpression: \(result)\n")}
        return result
    }
    
    
    /**********************************************************************/
    // PARSE SCRIPT
    
    
    func parse() throws -> EntoliScript { // parse full script
        var result = [Value]()
        while self.lexer.lookaheadBy(1) != nil {
            result.append(self.stripPeriod(try self.parseExpression()))
            if DEBUG {print("TOP-LEVEL EXPR: \(result.last)")}
            self.lexer.flush()
        }
        return EntoliScript(data: result)
    }
}


