//
//  punctuation-lexer.swift
//  entoli
//
//  This chunks code according to hardcoded tokens that never change (literal delimiters, punctuation, and whitespace), 
//  but not unquoted text (names, commands, operators, unquoted numbers and units, etc.) which is tokenized separately 
//  from UnquotedWords and their interstitial tokens using runtime-customizable data pattern and operator tables.
//

// note: an entoli script can contain three things: code, user docs (annotations) and developer docs (comments) // TO DO: in implementation terms, can comments safely be treated as a particular type of annotation, with different code delimiters but same the syntax rules and internal storage mechanism? Or do they need more flexibility/hygene, e.g. since they're often also used temporarily to block out broken/unfinished code during development and testing? (note: the same 3-prong approach - user+dev+trace info - should apply to error reporting)


// there might also be an argument for pushing vocab opdefs and numeric parsefuncs onto a stack, allowing ops to be added/removed mid-script; this does suggest that vocab parser should consume punc lexer's character stream directly (which is arguably simpler anyway, since it knows more about what it needs and can deal with e.g. periods as decimal separators without any messing about)


private let DEBUG = true


/**********************************************************************/

 
private struct PartialOperatorMatch<T:Hashable> {
    let precedingWords: [Lexer.UnquotedWord] // any preceding unquoted words already matched to LH side of match (if the operator is successfully matched, these will be added to cache as UnquotedName token, followed by the Operator token)
    let isLeftDelimited: Bool // is there an explicit delimiter on LH side of match? (if false, the operator definition's autoDelimit flag will be used)
    let startIndex: ScriptIndex // position in source code at which the whole match begins
    let endIndex: ScriptIndex // position in source code at which this partial/whole match ends
    let matchInfo: OperatorTable<T>.Part // struct describing the current char/word match along with lookup table for making the next char/word match
}


/**********************************************************************/


class Lexer {
    
    // TO DO: `!` and `?` punctuation could be very useful in REPL mode for altering evaluation behavior: `!` could indicate that a command with side-effects may be performed automatically by REPL whenever it likes (by default, effectful commands should always require explicit user action to initiate each time); `?` might perform a dry-run of commands and produce a report rather than result, or perhaps it could instruct entoli to ask user for help, pausing eval on that command so user can inspect or even modify its inputs, outputs, etc. (may be worth reserving both chars now, and currently just mapping to expression separator)
    
    /**********************************************************************/
    // Punctuation tokens (these are hardcoded and non-overrideable)
    
    // quoted text/name literals
    static let quoteDelimiters: [Character:TokenType] = [
        "\"": .QuotedText,
        "“": .QuotedText,
        "”": .QuotedText,
        "'": .QuotedName,
        "‘": .QuotedName,
        "’": .QuotedName,
    ]
    
    // annotation literals
    static let annotationDelimiters: [Character:Int] = [
        "«": 1,
        "»": -1,
    ]
    
    // collection literal/expression group delimiters, and separators
    static let punctuation: [Character:TokenType] = [
        "[": .ListLiteral,
        "]": .ListLiteralEnd,
        "{": .RecordLiteral,
        "}": .RecordLiteralEnd,
        "(": .ExpressionGroupLiteral,
        ")": .ExpressionGroupLiteralEnd,
        // TO DO: also consider making `!` and `?` expression separators; this would be mostly cosmetic as it wouldn't affect behavior, however, it could be used to infer metadata (e.g. `expr!` might indicate that user regards the operation as especially important, so could be highlighted differently, prioritized in searches, etc)
        ".": .ExpressionSeparator, // also decimal sep in canonical numbers (or thousands sep in localized numbers?)
        ",": .ItemSeparator, // also thousands separator in canonical numbers? (or decimal sep in localized numbers?)
        ":": .PairSeparator,
        ";": .PipeSeparator,
        // TO DO: should "@" and "#" also be fixed tokens? (used in `@mentions` and `#hashtag`, which may appear in code as well as in annotations; the former to identify universal [persistent machine-wide URI-like] resources, the latter to tag words for search indexing)
        "\n": .LineBreak,
        "\r": .LineBreak, // TO DO: how best to normalize CR/CRLF/LF? (note that this gets extra tricky inside text literals; prob best always to use LF internally and normalize at IO only; if user specifically wants CR, they should use [e.g.] ` "..." & 0u000C & "..."`)
    ]
    
    static let nonBreakingWhiteSpace: Set<Character> = [" ", "\t"]
    
    static let reservedCharacters = Set(quoteDelimiters.keys).union(Set(annotationDelimiters.keys))
        .union(Set(punctuation.keys)).union(Set(nonBreakingWhiteSpace)) // note: VocabularyTokenizer should never overload or redefine these tokens; e.g. to read an unquoted decimal number, it should look for digit[s]+ExpressionSeparator+digit[s]
    
    /**********************************************************************/
    // Initialization
    
    let code: ScriptChars
    var cursor: ScriptIndex
    let codeLength: ScriptIndex
    private let operatorsTable: Operators
    private let numericPrefixes: NumericPrefixes
    
    // TO DO: how would/should lexer tie into incremental parsing support? would the editor, given knowledge of the AST (which in turn lets it determine which token user is currently modifying), simply re-lex after each edit? also, bear in mind the need to lex as code is edited (which means lexer needs to be able to suspend and resume on input stream, which this implementation - which uses String, not stream - currently doesn't allow)
    // TO DO: if per-line parsing, option to indicate already inside quoted text/name [1] or annotation
    init(code: String, operatorsTable: Operators = StandardOperatorsTable, numericPrefixes: NumericPrefixes = []) {
        self.code = code.characters
        self.cursor = self.code.startIndex
        self.codeLength = self.code.endIndex
        self.operatorsTable = operatorsTable
        self.numericPrefixes = numericPrefixes
    }
    
    private var currentTokensCache: [Token] = [gStartOfCodeToken]
    
    private(set) var currentTokenIndex = 0 // parser can get this value and use it to backtrack to that token later on if required; caution: only valid until the next flush() call
    
    
    /**********************************************************************/
    // Tokenizer
    
    
    // read next token[s] into currentTokensCache; inserts gEndOfCodeToken once all tokens have been read
    private func readNextToken(ignoreVocabulary: Bool) {
        if DEBUG {print("READ_NEXT_TOKEN \(self.cursor) ==================================================")}
        if self.cursor < self.codeLength { // TO DO: what to do if overrun? (e.g. endlessly adding null tokens prob. isn't good idea if something gets stuck in infinite loop)
            let start = self.cursor
            let firstChar = self.code[self.cursor] // TO DO: prob make this `char` var and rejig stages into a single switch
            // 1. quoted text/name
            if let token = Lexer.quoteDelimiters[firstChar] { // found open quote
                var chars = String.CharacterView()
                var isquote = true
                while isquote {
                    self.cursor = self.cursor.successor() // eat open quote
                    let substart = self.cursor // [1] if already starting inside quote, need to initialize token, text, isquote, and start vars, skip eating open quote (since there isn't one), then continue from this line
                    while self.cursor < self.codeLength && Lexer.quoteDelimiters[self.code[self.cursor]] != token {
                        self.cursor = self.cursor.successor()
                    }
                    if self.cursor == self.codeLength {
                        chars.appendContentsOf(self.code[substart..<self.cursor])
                        return self.currentTokensCache.append(Token(type: token, value: String(chars), range: start..<self.cursor, partial: +1))
                    }
                    chars.appendContentsOf(self.code[substart..<self.cursor])
                    self.cursor = self.cursor.successor() // eat close quote
                    isquote = self.cursor < self.codeLength && Lexer.punctuation[self.code[self.cursor]] == token
                    if isquote { // note: quote chars are escaped by typing twice (the first is ignored, the second used)
                        chars.append(self.code[self.cursor])
                    }
                }
                return self.currentTokensCache.append(Token(type: token, value: String(chars), range: start..<self.cursor))
            }
            // 2. unquoted word (i.e. anything that isn't quoted, punctuation, or white space; though once called the read function may scan/consume those too where appropriate)
            if !Lexer.reservedCharacters.contains(firstChar) {
                if ignoreVocabulary {
                    self.readUnquotedName()
                } else {
                    self.readVocabulary()
                }
                return
            }
            // 3. 'non-breaking' white space; unlike LF, space and TAB do not delimit unquoted text but instead are treated as part of it; allowing the parser to reconstitute white space-separated words as a single token if/where/when it wishes
            while self.cursor < self.codeLength && Lexer.nonBreakingWhiteSpace.contains(self.code[self.cursor]) {
                self.cursor = self.cursor.successor()
            }
            if self.cursor > start {
                let range = start..<self.cursor
                return self.currentTokensCache.append(Token(type: .WhiteSpace, value: String(self.code[range]), range: range))
            }
            // 4. «...» annotations are atomic and nestable and read as-is (understanding their contents is left to the caller)
            if let t = Lexer.annotationDelimiters[firstChar] {
                if t < 0 { // TO DO: how to handle case where `»` is encountered first?
                    return self.currentTokensCache.append(Token(type: .AnnotationLiteralEnd, value: String(firstChar), range: start..<self.cursor))
                }
                let start = self.cursor
                var level = 1
                while level > 0 {
                    self.cursor = self.cursor.successor()
                    if self.cursor == self.codeLength {
                        let range = start..<self.cursor
                        return self.currentTokensCache.append(Token(type: .AnnotationLiteral, value: String(self.code[range]), range: range, partial: level))
                    }
                    level += Lexer.annotationDelimiters[self.code[self.cursor]] ?? 0
                }
                self.cursor = self.cursor.successor()
                let range = start..<self.cursor
                return self.currentTokensCache.append(Token(type: .AnnotationLiteral, value: String(self.code[range]), range: range))
            }
            // 5. all other tokens (single-char)
            if let token = Lexer.punctuation[firstChar] {
                self.cursor = self.cursor.successor()
                return self.currentTokensCache.append(Token(type: token, value: String(firstChar), range: start..<self.cursor))
            }
        }
        return self.currentTokensCache.append(gEndOfCodeToken)
    }
    
    
    /**********************************************************************/
     // Public interface
    
    
    var currentToken: Token { return self.currentTokensCache[self.currentTokenIndex] } // current token
    
    func advance(ignoreWhiteSpace: Bool = true, ignoreVocabulary: Bool = false) {
        repeat {
            self.currentTokenIndex += 1
            if self.currentTokenIndex == self.currentTokensCache.count { self.readNextToken(ignoreVocabulary) }
            //            if self.currentTokenIndex > 100 { print("BUG: punc.advance() is stuck: "+(self.currentTokensCache.map{String($0)}.joinWithSeparator("\n\t"))); break} // TO DO: DEBUG; DELETE
        } while ignoreWhiteSpace && self.currentToken.type == .WhiteSpace
    }
    
    func skip(tokenType: TokenType, ignoreWhiteSpace: Bool = true) throws { // advance to next token, throwing SyntaxError if it's not the specified type
        self.advance(ignoreWhiteSpace)
        if self.currentToken.type != tokenType {
            throw SyntaxError(description: "[0] Expected \(tokenType) but found \(self.currentToken.type)")
        }
    }
    
    func backtrackTo(tokenIndex: Int) { // note: technically this doesn't backtrack but rather moves to a previously read token (thus it could also be used to advance over previously parsed tokens for which cached Values have already been generated); might be an idea to rename it, or else replace with [safe] setter for currentTokenIndex
        self.currentTokenIndex = tokenIndex
    }
    
    // caution: lookahead doesn't know about operators/data detectors; it can only look for punctuation, whitespace, quoted name/text, and unquoted word
    func lookaheadBy(offset: UInt, ignoreWhiteSpace: Bool = true, ignoreVocabulary: Bool = false) -> Token? { // TO DO: what about annotations? (should prob. also ignore those by default, but need to confirm)
        if offset == 0 { return self.currentToken }
        var count: UInt = 0
        var lookaheadTokenIndex: Int = self.currentTokenIndex
        //if DEBUG {print("LOOKING AHEAD from \(self.currentToken) by \(offset)")}
        while count < offset {
            lookaheadTokenIndex += 1
            while lookaheadTokenIndex >= self.currentTokensCache.count { self.readNextToken(ignoreVocabulary) }
            let lookaheadToken = self.currentTokensCache[lookaheadTokenIndex]
            if lookaheadToken.type == .EndOfCode {
                if DEBUG {print("LOOKAHEAD REACHED END: \(self.currentTokensCache.map{$0.value as String!})")}
                return nil
            }
            if lookaheadToken.type != .WhiteSpace || !ignoreWhiteSpace { count += 1 }
        }
        //       print("LOOKED AHEAD to    \(self.currentTokensCache[lookaheadTokenIndex])")
        return self.currentTokensCache[lookaheadTokenIndex]
    }
    
    func flush() { // clear cache of fully-parsed tokens (at minimum, this leaves the current token at index 0)
        self.currentTokensCache.removeRange(0..<self.currentTokenIndex)
        self.currentTokenIndex = 0
    }

    
    /**********************************************************************/
    // Vocabulary readers (used by main tokenizer function to process unquoted word sequences)
    
    // supporting methods used by readVocabulary
    
    private typealias PartialPhraseOperatorMatch = PartialOperatorMatch<Operators.Phrase.Element>
    private typealias PartialSymbolOperatorMatch = PartialOperatorMatch<Operators.Symbol.Element>
    private typealias FullPhraseOperatorMatch    = (match: PartialPhraseOperatorMatch, isValidPrefixName: Bool, isValidInfixName: Bool)
    private typealias FullSymbolOperatorMatch    = (match: PartialSymbolOperatorMatch, isValidPrefixName: Bool, isValidInfixName: Bool)
    private typealias UnquotedWord               = (text: String, startIndex: ScriptIndex, endIndex: ScriptIndex)
    
    //
    
    private func joinWords(words: [UnquotedWord]) -> String {
        return words.map{$0.text}.joinWithSeparator(" ")
    }
    
    private func wordsRange(words: [UnquotedWord]) -> ScriptRange {
        return words.first!.startIndex..<words.last!.endIndex
    }
    
    private func addUnquotedName(words: [UnquotedWord]) {
        if DEBUG {if words.count == 0 {print("BUG: can't addName (zero-length)")}; print("FOUND NAME: <\(self.joinWords(words))>     range=\(self.wordsRange(words))")}
        self.currentTokensCache.append(Token(type: .UnquotedName, value: self.joinWords(words), range: self.wordsRange(words)))
    }
    
    // determine if a matched char/word sequence is a valid operator name (i.e. it's self-delimiting or not bounded by additional chars/words)
    
    private func isEndOfPhrase() -> Bool { // used by isValidOperatorName() to determine if there are any more words in current unquoted words sequence
        var idx = self.cursor
        repeat {
            idx = idx.successor()
        } while idx < self.codeLength && Lexer.nonBreakingWhiteSpace.contains(self.code[idx])
        if idx == self.codeLength || Lexer.reservedCharacters.contains(self.code[idx]) { return true }
        // there is a next word, so determine whether or not it's a numeric (numeric words cannot appear in operators or unquoted names)
        return isNumericWord(self.code, start: idx, allowPrefixes: self.numericPrefixes)
    }
    
    private func isEndOfWord() -> Bool {
        let idx = self.cursor.successor()
        return idx == self.codeLength || Lexer.reservedCharacters.contains(self.code[idx])
    }
    
    private func isValidOperatorName(operatorDefinition: OperatorDefinition?, isLeftDelimited: Bool, isRightDelimitedFunc: ()->Bool) -> Bool { // check if matched word[s] are 1. a complete operator name, and 2. delimited as per operator definition's requirements
        if let opDef = operatorDefinition { // non-nil if the operator name has been fully matched
            // note: if the matched "operator" is NOT left-self-delimiting, it must be first in word sequence; conversely, if it is NOT right-self-delimiting, it must be last. If these conditions are not met then it is not an operator, just normal word[s] within a longer phrase. e.g. The `to` prefix operator is not left-self-delimiting, so `to foo` is a valid `to` op with 'foo' as its RH operand, but `go to` is just an ordinary name (i.e. the 'to' is not special as it is not the first word in the expression).
            return (isLeftDelimited || opDef.name.autoDelimit.left) && (opDef.name.autoDelimit.right || isRightDelimitedFunc())
        }
        return false
    }
    
    
    // note: symbol ops can't be added to cache until current word is fully read, allowing keyword op first refusal; therefore, this needs to return any preceding words as UnquotedName token, followed by Operator token; or nil if no full match was made
    
    private func updateOperatorMatches<T:Hashable>(operatorsTable: OperatorTable<T>,
                                                   inout partialOperatorMatches: [PartialOperatorMatch<T>],
                                                   inout fullOperatorMatch: (match: PartialOperatorMatch<T>, isValidPrefixName: Bool, isValidInfixName: Bool)?, // once first full match is made, it is cached here, and no later operators are matched
                                                   value: T, endIndex: ScriptIndex, precedingWords: [UnquotedWord],
                                                   isLeftDelimited: Bool, isRightDelimitedFunc: ()->Bool) -> (Token?, Token)? { // returns true if operator found
    //    if DEBUG {print("Checking \(T.self) operator matches: `\(value)` partial=\(partialOperatorMatches.count), full=\(fullOperatorMatch)")}

        // check each current partial operator match; if a full valid operator name is matched, proceed to next stage; otherwise update or discard each partial match depending on whether or not it continues to match on current word
        var partialMatchIndex = 0
        while partialMatchIndex < partialOperatorMatches.count { // partial matches are ordered from earliest to latest; goal is to find the earliest, longest full match
            let match = partialOperatorMatches[partialMatchIndex]
            // check if we've found a complete, valid (i.e. correctly delimited) operator name
            
            
            
            // TO DO: this is too eager; it needs to wait until isLongest is reached for earliest match
            let isValidPrefixOperatorName = self.isValidOperatorName(match.matchInfo.prefixDefinition, isLeftDelimited: match.isLeftDelimited, isRightDelimitedFunc: isRightDelimitedFunc)
            let isValidInfixOperatorName = self.isValidOperatorName(match.matchInfo.infixDefinition, isLeftDelimited: match.isLeftDelimited, isRightDelimitedFunc: isRightDelimitedFunc)
            // TO DO: fullMatch needs to indicate which name(s) are valid; when final best match is made, only include the op(s) that are valid (i.e. different op names, including aliases, can have different delimiting)
            if isValidPrefixOperatorName || isValidInfixOperatorName { // found a full, correctly delimited operator name
                fullOperatorMatch = (match, isValidPrefixOperatorName, isValidInfixOperatorName)
                partialOperatorMatches = [match]
                partialMatchIndex = 0
                if DEBUG {print("FOUND A FULL MATCH: \(fullOperatorMatch!)\n")}
            }
            // update current partial matches
            if DEBUG {print("MATCHING NEXT \(T.self): `\(value)` in `\(match.matchInfo.nextWords.map{$0.0})`")}
            if let nextMatch = match.matchInfo.nextWords[value] {
                partialOperatorMatches[partialMatchIndex] = PartialOperatorMatch<T>(precedingWords: match.precedingWords, // TO DO: need to add value to precedingWords here!
                                                                                    isLeftDelimited: match.isLeftDelimited,
                                                                                    startIndex: match.startIndex, endIndex: endIndex,
                                                                                    matchInfo: nextMatch)
                if DEBUG {print("... and matched it too: \(partialOperatorMatches[partialMatchIndex])")}
                partialMatchIndex += 1
            } else {
                if DEBUG {print("...and failed to make a match, so discarding partial.")}
                partialOperatorMatches.removeAtIndex(partialMatchIndex)
            }
        }
        if fullOperatorMatch == nil { // check for the start of a new operator (note: this only needs done until the first full match is made)
            if let foundOp = operatorsTable.definitionsByPart[value] { // matched the first char/word of a possible operator
                partialOperatorMatches.append(PartialOperatorMatch<T>(precedingWords: precedingWords, isLeftDelimited: isLeftDelimited,
                    startIndex: self.cursor, endIndex: endIndex, matchInfo: foundOp))
                print("Got a new operator match: \(foundOp)")
            }
        } else if fullOperatorMatch!.match.matchInfo.isLongest || partialOperatorMatches.count == 0 { // check if longest full match has been made
            var nameToken: Token? = nil
            let match = fullOperatorMatch!.match
            if match.precedingWords.count > 0 {
                nameToken = Token(type: .UnquotedName, value: self.joinWords(match.precedingWords), range: self.wordsRange(match.precedingWords))
            }
            //print(precedingWords)
            if DEBUG {print("FULLY MATCHED \(T.self) OPERATOR: <\(match.matchInfo.name)>     range=\(match.startIndex..<match.endIndex)")}
            return (nameToken, Token(type: .Operator, value: match.matchInfo.name!, range: match.startIndex..<match.endIndex,
                operatorDefinitions: (match.matchInfo.prefixDefinition, match.matchInfo.infixDefinition))) // TO DO: FIX!!!!! only include matches with valid name flag
        }
        return nil
    }
    
    // readers
    
    
    private func readVocabulary() {
        // note: backtracking here means using source character indexes; this is best when opportunistically parsing record items, where parser should look first for unquoted name followed by colon (a `literal-name:any-value` pair), then backtracking, switching vocab mode and retrying if the colon isn't found (one could even argue for implementing that particular search as a custom `currentTokenAsRecordItemName` method on lexer that returns either the unquoted name token or nil, avoiding need to expose another backtracking API)
        
        // other backtracking uses: opportunistic parsing of command argument, where an unsuitable token is left as-is to be read on next pass (i.e. no sense in throwing that work away); if parseAtom finds an operator that turns out to be infix (in which case it needs to backtrack in case it's reading argument to command, in which case infix op will use command name as leftExpr -- although won't parseArgument do that anyway?)
        
        
        // scan all words until the first full [and longest] operator match is made (caveat: if it's not left and right auto-delimiting, need to discard if there are adjoining words)
        var words = [UnquotedWord]() // used to construct command name, if first word[s] are not numeric word or operator name
        var partialPhraseOperatorMatches = [PartialPhraseOperatorMatch]()
        if DEBUG {print("******* START READ VOCAB \(self.cursor)")}
        var isFirstWord = true
        var fullPhraseOperatorMatch: FullPhraseOperatorMatch? = nil
        while self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor]) { // TO DO: not sure about this; will see (might be better as repeat...while, or as break)
            let wordStartIndex = self.cursor
            
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // pattern match current word for numbers, units, currency, etc. 
            // (note: this is done before operator matching to ensure operators cannot redefine numeric words)
            // TO DO: how to avoid confusion between prefix/suffix chars in numeric word and symbol operators?
            // check for numeric word
            if let (numericValue, range) = readNumericChars(self.code, start: wordStartIndex, allowPrefixes: self.numericPrefixes,
                                                                                         nonWordCharacters: Lexer.reservedCharacters) {
                
                if words.count > 0 { // complete and cache tokens for all preceding chars
                    self.addUnquotedName(words) // TO DO: FIX!!! need to add any matched ops and interstitial names prior to adding numeric (currently, this justs adds everything as a single unquoted name, which is wrong); oh, and also need to think about how symbol ops - esp. when in-word - interact with numerics, particularly their unit suffixes (i.e. prob. need to move suffix parsing into its own function to be called after the readNumericChars function, thus allowing symbol operator matching to be done on it first, e.g. `2.5g*n`)
                }
                
                self.cursor = range.endIndex // TO DO: confirm this positions correctly
                let token = Token(type: .NumericWord, value: String(self.code[range]), range: wordStartIndex..<self.cursor, numericInfo: numericValue)
                //if DEBUG {print("READ NUMERIC: \(token)")}
                self.currentTokensCache.append(token)
                return
            }
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // pattern match for symbol and phrase operators
            var wordChars = String.CharacterView()
            var bestChoiceOperator: (precedingNameToken: Token?, operatorToken: Token)? = nil
            // match in-word symbol operators as the word is being read, then match phrase operators once the word is complete
            // TO DO: since symbol ops are read first, there's no point adding them to phrase ops table as well, so can remove that duplication and related logic in ops-table and isValidOp()
            var partialSymbolOperatorMatches = [PartialSymbolOperatorMatch]()
            var fullSymbolOperatorMatch: FullSymbolOperatorMatch? = nil
            
            var isFirstChar = true
            repeat {
                let char = self.code[self.cursor]
                if bestChoiceOperator == nil {
                    bestChoiceOperator = updateOperatorMatches(self.operatorsTable.symbols, partialOperatorMatches: &partialSymbolOperatorMatches,
                                                              fullOperatorMatch: &fullSymbolOperatorMatch, value: char, endIndex: self.cursor,
                                                              precedingWords: words, isLeftDelimited: isFirstChar, isRightDelimitedFunc: self.isEndOfWord)
                    if (bestChoiceOperator != nil) {print("MATCHED SYMBOL OPERATOR!!!!", bestChoiceOperator)}
                }
                isFirstChar = false
                wordChars.append(char)
                self.cursor = self.cursor.successor()
            } while self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor])
            let word = String(wordChars)
            let wordEndIndex = self.cursor // caution: non-inclusive; use `..<` (not `...`) to construct ScriptRange
            isFirstWord = false
            if let fullMatch = updateOperatorMatches(self.operatorsTable.phrases, partialOperatorMatches: &partialPhraseOperatorMatches,
                                                                   fullOperatorMatch: &fullPhraseOperatorMatch, value: word, endIndex: wordEndIndex,
                                                                   precedingWords: words, isLeftDelimited: isFirstWord, isRightDelimitedFunc: self.isEndOfPhrase) {
                bestChoiceOperator = fullMatch // note: while symbol ops are matched first, while word is still being read, phrase
                words.removeAll()
            }
            words.append((word, wordStartIndex, wordEndIndex))
            if DEBUG {print("bestChoiceOperator=",bestChoiceOperator,"   \n\t\tcurrent-words:",words)}
            if let (precedingNameToken, operatorToken) = bestChoiceOperator {
                if precedingNameToken != nil {
                    if DEBUG {print("CACHING PRECEDING NAME \(precedingNameToken!)")}
                    self.currentTokensCache.append(precedingNameToken!)
                }
                if DEBUG {print("CACHING SYMBOL OPERATOR \(operatorToken)")}
                self.currentTokensCache.append(operatorToken)
                
                // TO DO: move cursor back to end of op
                self.cursor = operatorToken.range.endIndex
                //print("...MOVED CURSOR BACK TO: \(self.cursor)")
                return
            }
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // skip over whitespace
            let n = self.cursor
            repeat {
                self.cursor = self.cursor.successor()
            } while self.cursor < self.codeLength && Lexer.nonBreakingWhiteSpace.contains(self.code[self.cursor])
            if DEBUG {print("******* SKIPPING WHITE SPACE \(n)..<\(self.cursor)")}
        }
        
        if DEBUG {print("******* END OF VOCAB LOOP \(self.cursor)\n\n\t\(partialPhraseOperatorMatches)\n\n\t\(words)")}
        
        // TO DO: need to sort this out; also need to check partialPhraseOperatorMatches for any completed matches (if an op is found, need to add its preceding name, if any, then op itself, then figure out how much of leftover words are to be added)
        
        if words.count > 0 {
            self.addUnquotedName(words)
            self.cursor = words.last!.endIndex
        }
    }
    
    
    
    private func readUnquotedName() {
        // TO DO: implement; read all whitespace-separated words and append .UnquotedName token
        var words = [String]()
        let startIndex = self.cursor
        var endWordIndex: ScriptIndex
        repeat {
            let startWordIndex = self.cursor
            // read to end of word
            repeat {
                self.cursor = self.cursor.successor()
            } while self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor])
            words.append(String(self.code[startWordIndex..<self.cursor]))
            endWordIndex = self.cursor
            repeat {
                self.cursor = self.cursor.successor()
            } while self.cursor < self.codeLength && Lexer.nonBreakingWhiteSpace.contains(self.code[self.cursor])
        } while self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor])
        self.cursor = endWordIndex // TO DO: confirm this positions correctly
        return self.currentTokensCache.append(Token(type: .UnquotedName, value: words.joinWithSeparator(" "), range: startIndex..<endWordIndex))
    }
}


