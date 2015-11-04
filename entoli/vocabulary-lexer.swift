//
//  vocabulary-lexer.swift
//  entoli
//
//

// TO DO: split NumericWord into Number and Quantity? (Q. how to validate quantities?) // bear in mind that any numeric info will be attached to TextValue; it's not necessary to generate that info here as typespecs will add full info if not already present; all we _really_ need to do here is determine it's some sort of numeric value, which means it starts with a digit, with optional `-` or `+` prefix, plus any additional optional prefixes (which will have to be specified some way, either as hardcoded unicode ranges for e.g. currency, or by NumericUnit typespecs that know how to read those values - in which case they will also declare which prefixes and suffixes they recognize, allowing fast identification and categorization [currency, weight, length, temperature, etc])


// someone (lexer/parser) needs to hand off to this whenever an UnquotedWord is encountered; this will check the entire sequence of unquoted words, including related tokens such as ExpressionSeparator (`.`) when it's being used as a decimal separator in decimal numeric


// simplest option *might* be for lexer 1 to allow its tokens to be rewritten by lexer 2, [although we might still need to keep originals in case we need to backtrack and reparse] (e.g. record item names should be initially read as words on assumption of finding colon, and if not found then backtrack...oh, this is a problem for record in `to` op, which always requires names, and must ignore ops and numerics - one could argue for that particular record requiring non-standard parsing; another possibility would be for record items not to be parsed in detail until actually consumed, at which point proc can dictate how to parse it [things could be dicey though since braces need correctly balanced and we'll prob. want to count commas as well, which makes even punctuation-only parsing tricky])

// actually, simplest (if not most efficient) is to read unquoted names as both simple NameValue and as vocab code (oh, except that's problematic too, since period seps get consumed by latter)


private let DEBUG = false


class VocabularyLexer {
    
    // TO DO: use same structure as punc lexer, with struct value being enum? (unconvinced: mostly that just adds complexity, and it's reasonable to assume parser will deal with vocab tokens by passing them to a switch that returns corresponding Value)

    /**********************************************************************/
    
    private let lexer: PunctuationLexer
    private let keywordOperators: OperatorPhrasesTable
    
    init(lexer: PunctuationLexer, keywordOperators: OperatorPhrasesTable) {
        self.lexer = lexer
        self.keywordOperators = keywordOperators
    }
    
    /**********************************************************************/
    // PARSE WORD [SEQUENCE]
    
    private typealias PartialOperatorPhraseMatch = (words: [Token], startIndex: ScriptIndex, match: OperatorPhrasesTable.WordInfoType)

    // support
    
    private var hasNextWord: Bool { return self.lexer.lookaheadBy(1, ignoreWhiteSpace: false)?.type == .WhiteSpace
                                                                 && self.lexer.lookaheadBy(1)?.type == .UnquotedWord }
    
    private func joinWords(words: [Token]) -> String {
        return words.map{$0.value}.joinWithSeparator(" ")
    }
    
    private func wordsRange(words: [Token]) -> ScriptRange {
        return words.first!.range.startIndex..<words.last!.range.endIndex
    }
    
    private func addName(words: [Token]) {
        if DEBUG {if words.count == 0 {print("BUG: can't addName (zero-length)")}; print("FOUND NAME: <\(self.joinWords(words))>     range=\(self.wordsRange(words))")}
        self.currentTokensCache.append(Token(type: .UnquotedName, value: self.joinWords(words), range: self.wordsRange(words)))
    }
    
    // operator
    
    private func isValidOperatorName(operatorDefinition: OperatorDefinition?, isFirstWord: Bool) -> Bool { // check if matched word[s] are delimited as per operator definition's requirements
        // note: if the matched "operator" is NOT left-self-delimiting, it must be first in word sequence; conversely, if it is NOT right-self-delimiting, it must be last. If these conditions are not met then it is not an operator, just normal word[s] within a longer phrase. e.g. The `to` prefix operator is not left-self-delimiting, so `to foo` is a valid `to` op with 'foo' as its RH operand, but `go to` is just an ordinary name (i.e. the 'to' is not special as it is not the first word in the expression).
        if let opDef = operatorDefinition {
            let autoDelimit = opDef.name.type == .Symbol ? .Full : opDef.name.autoDelimit // (whitespace _always_ delimits symbol-based operators)
            return (autoDelimit.left || isFirstWord) && (autoDelimit.right || !self.hasNextWord) // TO DO: `hasNextWord` test isn't quite right as numeric words _always_ self-delimit; leave it for now, but fix/replace when numerics support is implemented
        }
        return false
    }
    
    // numeric
    
    private static let numericDigits = Set("01234567890".characters)
    private static let numericPrefixes = Set("+-".characters)
    
    private func readNumericWord(word: String, prefixChars: Set<Character> = [], digitChars: Set<Character> = numericDigits) -> String? {
        let chars = word.characters
        var idx = chars.startIndex
        while idx < chars.endIndex && prefixChars.contains(chars[idx]) {
            idx = idx.successor()
        }
        if !(idx < chars.endIndex && digitChars.contains(chars[idx])) { return nil } // is it a numeric word?
         // next, scan to last digit
        
        return word
    }
    
    // re-tokenizer; reads UnquotedWord sequences (including related tokens) from PunctuationLexer, and converts them into VocabularyLexer.Tokens

    private func _next() { // match next numeric word/keyword operator/unquoted name, reading ahead and caching additional tokens as necessary
        if self.isDone || self.lexer.currentToken == nil {
            self.currentTokensCache.append(nil)
            return
        }
        // scan all words until the first full [and longest] operator match is made (caveat: if it's not left and right auto-delimiting, need to discard if there are adjoining words)
        var words = [Token]() // used to construct command name, if first word[s] are not numeric word or operator name
        var partialOperatorPhraseMatches = [PartialOperatorPhraseMatch]()
        while self.lexer.currentToken?.type == .UnquotedWord {
            let token = self.lexer.currentToken!
            let word = token.value
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // pattern match current word for numbers, units, currency, etc. (note: this comes before operator checks, ensuring operators cannot redefine numeric words)
            if let result = readNumericWord(word, prefixChars: VocabularyLexer.numericPrefixes) {
                
                if words.count > 0 { self.addName(words) }
                if DEBUG {print("FOUND NUMBER: \(result)     range=\(token.range.startIndex..<self.lexer.currentToken!.range.endIndex)")}
                self.currentTokensCache.append(Token(type: .NumericWord, value: result, range: token.range.startIndex..<self.lexer.currentToken!.range.endIndex))
                return
            }
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // check for the start of a new keyword operator
            if let foundOp = self.keywordOperators.definitionsByWord[token.value] { // matched the first word of a possible keyword operator
                partialOperatorPhraseMatches.append((words, self.lexer.currentToken!.range.startIndex, foundOp)) // TO DO: this should capture (precedingWords:words,literalOperatorNameStart:token.range.start,...)
            } else {
                ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
                // TO DO: pattern match current word for symbol operators (also, check this should definitely come after keyword op check; note that keyword op check will pick up all space-delimited symbol ops too)
                // TO DO: symbol operator matching needs to be done here; not sure what's quickest; however, it's worth bearing in mind that if a full symbol operator match is made then we've hit the end condition for searching and need to proceed to determining if we've correct operands, etc, same as for keyword op
                ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            }
            // check each current partial operator match; if a full valid operator name is matched, proceed to next stage; otherwise update or discard each partial match depending on whether or not it continues to match on current word
            var partialMatchIndex = 0
            while partialMatchIndex < partialOperatorPhraseMatches.count { // partial matches are ordered from earliest to latest; goal is to find the earliest, longest full match
                let (precedingWords, startIndex, match) = partialOperatorPhraseMatches[partialMatchIndex]
                // check if we've found a complete, valid (i.e. correctly delimited) operator name
                let isFirstWord = precedingWords.count == 0
                if self.isValidOperatorName(match.prefixDefinition, isFirstWord: isFirstWord)
                        || self.isValidOperatorName(match.infixDefinition, isFirstWord: isFirstWord) { // found a full, correctly delimited operator name
                    if !isFirstWord {
                        self.addName(precedingWords)
                    }
                    if DEBUG {print("FOUND OPERATOR: <\(match.name)>     range=\(startIndex..<self.lexer.currentToken!.range.endIndex)")}
                            self.currentTokensCache.append(Token(type: .Operator, value: "",
                                                                 range: startIndex..<self.lexer.currentToken!.range.endIndex,
                                                                 operatorDefinition: (match.prefixDefinition, match.infixDefinition)))
                    return
                }
                // update current partial matches
                if let nextMatch = match.nextWords[token.value] {
                    partialOperatorPhraseMatches[partialMatchIndex] = (precedingWords, startIndex, nextMatch)
                    partialMatchIndex += 1
                } else {
                    partialOperatorPhraseMatches.removeAtIndex(partialMatchIndex)
                }
            }
            words.append(token)
            if !self.hasNextWord { // no numerics or operators were found, so treat entire word sequence as unquoted name
                self.isDone = true
                self.currentTokensCache.append(Token(type: .UnquotedName, value: self.joinWords(words), range: self.wordsRange(words)))
                return
            }
            self.lexer.advance() // advance to next word
        }
        self.currentTokensCache.append(nil)
    }

    
    // parse a sequence of unquoted words (name/number/operation)
    
    private var currentTokensCache: [Token?] = [nil]
    private(set) var currentTokenIndex = 0
    private(set) var isDone = false // internal flag set when end of word sequence is reached; must be reset before next word sequence can start
    
    var currentToken: Token? { return self.currentTokensCache[self.currentTokenIndex] } // current token (or nil once end of word sequence is reached)
    
    func advance() {
        self.currentTokenIndex += 1
        if self.currentTokenIndex == self.currentTokensCache.count { self._next() }
        if DEBUG {if self.currentTokenIndex >= self.currentTokensCache.count {print("\nBUG: vocab lexer advanced to \(self.currentTokenIndex), but has read only:"); for (i,t) in self.currentTokensCache.enumerate() { print("\t\(i).\t", t) }; print("")}}
    }
    
    func lookaheadBy(offset: UInt) -> Token? { // TO DO: what about annotations? (should prob. also ignore those by default, but need to confirm)
        if offset == 0 { return self.currentToken }
        let lookaheadTokenIndex = self.currentTokenIndex + Int(offset)
        //if DEBUG {print("LOOKING AHEAD from \(self.currentToken) by \(offset)")}
        while lookaheadTokenIndex >= self.currentTokensCache.count { self._next() }
        if self.currentTokensCache[lookaheadTokenIndex] == nil {
            if DEBUG {print("LOOKAHEAD REACHED END: \(self.currentTokensCache)")}
            return nil
        }
        //if DEBUG {print("LOOKED AHEAD to    \(self.currentTokensCache[lookaheadTokenIndex])")}
        return self.currentTokensCache[lookaheadTokenIndex]
    }
    
    // note: flush must be called before vocab lexer can be reused
    
    func flush() { // clear cache of fully-parsed tokens
        self.currentTokensCache = [nil]
        self.currentTokenIndex = 0
        self.isDone = false
    }
    
    
    
    
    
    /*
    
    func skip(tokenType: TokenType, ignoreWhiteSpace: Bool = true) throws { // advance to next token, throwing SyntaxError if it's not the specified type
        self.advance(ignoreWhiteSpace)
        if let token = self.currentToken {
            if token.type != tokenType {
                throw SyntaxError(description: "[0] Expected \(tokenType) but found \(token.type)")
            }
        }
    }
    
    func backtrackTo(tokenIndex: Int) { // note: technically this doesn't backtrack but rather moves to a previously read token (thus it could also be used to advance over previously parsed tokens for which cached Values have already been generated); might be an idea to rename it, or else replace with [safe] setter for currentTokenIndex
        self.currentTokenIndex = tokenIndex
    }
    
    */
}

    