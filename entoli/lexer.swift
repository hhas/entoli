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


private let DEBUG = false


/**********************************************************************/

 
private struct PartialOperatorMatch<T:Hashable> {
    let precedingWords: [Lexer.UnquotedWord] // any preceding unquoted words already matched to LH side of match (if the operator is successfully matched, these will be added to cache as UnquotedName token, followed by the Operator token)
    let currentPartialWord: Lexer.PartialWord?
    let isLeftDelimited: Bool // is there an explicit delimiter on LH side of match? (if false, the operator definition's autoDelimit flag will be used)
    let startIndex: ScriptIndex // position in source code at which the whole match begins
    let endIndex: ScriptIndex // position in source code at which this partial/whole match ends
    let info: OperatorTable<T>.Part // struct describing the current char/word match along with lookup table for making the next char/word match
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
        // TO DO: also consider making `!` and `?` expression separators; this would be mostly cosmetic as it wouldn't affect behavior, however, it could be used to infer metadata (e.g. `expr!` might indicate that user regards the operation as especially important, so could be highlighted differently, prioritized in searches, etc); caution: `!=` is defined as ASCII equivalent for `≠` operator, so would require [e.g.] leading whitespace to disambiguate (e.g. see also `.` overloading in numerics; might be worth considering a general scheme)
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
    
    private(set) var currentTokensCache: [Token] = [gStartOfCodeToken] // TO DO: DEBUG; make fully private
    
    private(set) var currentTokenIndex = 0 // parser can get this value and use it to backtrack to that token later on if required; caution: only valid until the next flush() call
    
    
    /**********************************************************************/
    // Tokenizer
    
    
    // read next token[s] into currentTokensCache; inserts gEndOfCodeToken once all tokens have been read
    private func readNextToken(ignoreVocabulary: Bool) {
        if DEBUG {print("READ_NEXT_TOKEN \(self.cursor) ==================================================")}
        // skip any leading 'non-breaking' white space (note: unlike LF, space and TAB do not automatically delimit unquoted text but instead can be treated as part of it, allowing the lexer to combine white space-separated words as a single .Operator/.UnquotedName token if/where/when it wishes. Inter-word tabs/spaces will be read and normalized by the relevant `Lexer.read...` method; tab/space runs appearing elsewhere in the code delimit tokens normally and are otherwise ignored.)
        while self.cursor < self.codeLength && Lexer.nonBreakingWhiteSpace.contains(self.code[self.cursor]) {
            self.cursor = self.cursor.successor()
        }
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
                let charIndex = self.cursor
                let char = self.code[charIndex]
                //print("READ VOCAB CHAR `\(char)`")
                self.cursor = self.cursor.successor()
                if bestChoiceOperator == nil {
                    
                    
                    // TO DO: BUG: .UnquotedName currently only contains preceding words, not preceding chars in current word; simplest solution is to pass wordChars as partialWord
                    
                    
                    bestChoiceOperator = updateOperatorMatches(self.operatorsTable.symbols,
                                                               partialMatches: &partialSymbolOperatorMatches, fullMatch: &fullSymbolOperatorMatch,
                                                               value: char, startIndex: charIndex, endIndex: self.cursor,
                                                               precedingWords: words, currentPartialWord: (wordChars, wordStartIndex),
                                                               isLeftDelimited: isFirstChar, isRightDelimitedFunc: self.isEndOfWord)
                    if DEBUG {if (bestChoiceOperator != nil) {print("MATCHED SYMBOL OPERATOR!!!!", bestChoiceOperator)}}
                }
                isFirstChar = false
                wordChars.append(char)
            } while self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor])
            let word = String(wordChars)
            let wordEndIndex = self.cursor // caution: non-inclusive; use `..<` (not `...`) to construct ScriptRange
            isFirstWord = false
            if let fullMatch = updateOperatorMatches(self.operatorsTable.phrases,
                                                     partialMatches: &partialPhraseOperatorMatches, fullMatch: &fullPhraseOperatorMatch,
                                                     value: word, startIndex: wordStartIndex, endIndex: wordEndIndex,
                                                     precedingWords: words, currentPartialWord: nil,
                                                     isLeftDelimited: isFirstWord, isRightDelimitedFunc: self.isEndOfPhrase) {
                    bestChoiceOperator = fullMatch // note: while symbol ops are matched first, while word is still being read, phrase
                    words.removeAll()
                    self.cursor = self.cursor.successor()
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
                let n = self.cursor
                self.cursor = operatorToken.range.endIndex
                if DEBUG {print("...MOVED CURSOR FROM \(n) BACK TO: \(self.cursor), `\(self.code[self.cursor])`")}
                return
            }
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // skip over whitespace
            let n = self.cursor
            while self.cursor < self.codeLength && Lexer.nonBreakingWhiteSpace.contains(self.code[self.cursor]) {
                //print("SKIP c\(self.cursor)")
                self.cursor = self.cursor.successor()
            } 
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
        var endWordIndex: ScriptIndex = startIndex
        while self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor]) {
            let startWordIndex = self.cursor
            // read to end of word
            while self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor]) {
                self.cursor = self.cursor.successor()
            } 
            words.append(String(self.code[startWordIndex..<self.cursor]))
            endWordIndex = self.cursor
            while self.cursor < self.codeLength && Lexer.nonBreakingWhiteSpace.contains(self.code[self.cursor]) {
                self.cursor = self.cursor.successor()
            } 
        } 
        self.cursor = endWordIndex // TO DO: confirm this positions correctly
        //print("READ UNQUOTED NAME \"\(String(self.code[startIndex..<endWordIndex]))\" \(startIndex..<endWordIndex)")
        return self.currentTokensCache.append(Token(type: .UnquotedName, value: words.joinWithSeparator(" "), range: startIndex..<endWordIndex))
    }
    
    
    
    /**********************************************************************/
     // Vocabulary readers (used by main tokenizer function to process unquoted word sequences)
     
     // supporting methods used by readVocabulary
    private typealias PartialPhraseOperatorMatch = PartialOperatorMatch<Operators.Phrase.Element>
    private typealias PartialSymbolOperatorMatch = PartialOperatorMatch<Operators.Symbol.Element>
    private typealias OperatorMatchType          = (prefix: Bool, infix: Bool)
    private typealias FullPhraseOperatorMatch    = (match: PartialPhraseOperatorMatch, type: OperatorMatchType)
    private typealias FullSymbolOperatorMatch    = (match: PartialSymbolOperatorMatch, type: OperatorMatchType)
    
    private typealias UnquotedWord               = (text: String, startIndex: ScriptIndex, endIndex: ScriptIndex)
    private typealias PartialWord                = (chars: String.CharacterView, startIndex: ScriptIndex)
    
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
    
    // TO DO: parameterize these: they shouldn't rely on cursor's current position but should instead take current match's endIndex as arg
    
    private func isEndOfPhrase() -> Bool { // used by isValidOperatorName() to determine if there are any more words in current unquoted words sequence (note: this does not test if subsequent word is a left-delimited operator, which also needs to be done if this test returns false)
        var idx = self.cursor
        repeat {
            idx = idx.successor()
        } while idx < self.codeLength && Lexer.nonBreakingWhiteSpace.contains(self.code[idx])
        return idx == self.codeLength || Lexer.reservedCharacters.contains(self.code[idx])
            || isNumericWord(self.code, start: idx, allowPrefixes: self.numericPrefixes)
    }
    
    private func isEndOfWord() -> Bool { // as above, returns false if there are still characters to be consumed, requiring an additional test to disambiguate adjoining operators
        let idx = self.cursor.successor()
        return idx == self.codeLength || Lexer.reservedCharacters.contains(self.code[idx])
    }
    
    private var isAutoLeftDelimitedOperatorNext: Bool {
        print("CAUTION: Auto-delimiting doesn't yet work for adjoining operators; code may parse incorrectly as a result.")
        return true             // TO DO: fix; check if next char[s]/word[s] can form an operator; if they do, and they're auto-delimiting, then return true
        
    }
    
    private func isValidOperatorName(operatorDefinition: OperatorDefinition?, isLeftDelimited: Bool, isRightDelimitedFunc: ()->Bool) -> Bool { // check if matched word[s] are 1. a complete operator name, and 2. delimited as per operator definition's requirements
        if let opDef = operatorDefinition { // non-nil if the operator name has been fully matched
            // note: if the matched "operator" is NOT left-self-delimiting, it must be first in word sequence; conversely, if it is NOT right-self-delimiting, it must be last. If these conditions are not met then it is not an operator, just normal word[s] within a longer phrase. e.g. The `to` prefix operator is not left-self-delimiting, so `to foo` is a valid `to` op with 'foo' as its RH operand, but `go to` is just an ordinary name (i.e. the 'to' is not special as it is not the first word in the expression).
            return (isLeftDelimited || opDef.name.autoDelimit.left)
                && (opDef.name.autoDelimit.right || isRightDelimitedFunc() || self.isAutoLeftDelimitedOperatorNext)
        }
        return false
    }
    
    
    /**********************************************************************/
     // note: symbol ops can't be added to cache until current word is fully read, allowing keyword op first refusal; therefore, this needs to return any preceding words as UnquotedName token, followed by Operator token; or nil if no full match was made
    
    
    private func updateOperatorMatches<T:Hashable>(operatorsTable: OperatorTable<T>,
                                                   inout partialMatches: [PartialOperatorMatch<T>],
                                                   inout fullMatch: (match: PartialOperatorMatch<T>, type: OperatorMatchType)?, // once first full match is made, it is cached here, and no later operators are matched
                                                   value: T, startIndex: ScriptIndex, endIndex: ScriptIndex,
                                                   precedingWords: [UnquotedWord], currentPartialWord: PartialWord?,
                                                   isLeftDelimited: Bool, isRightDelimitedFunc: ()->Bool) -> (Token?, Token)? { // once the best operator match is made, returns .UnquotedName token for preceding words (if any) and the finished .Operator token
        //    if DEBUG {print("Checking \(T.self) operator matches: `\(value)` partial=\(partialOperatorMatches.count), full=\(fullMatch)")}
        // check for the start of a new operator (note: this only needs done until the first full match is made)
        
        //print(">>> `\(value)`, \(precedingWords)")
                                                
        if fullMatch == nil {
            if let matchInfo = operatorsTable.definitionsByPart[value] { // matched the first char/word of a possible operator
                let isValidPrefixOperatorName = self.isValidOperatorName(matchInfo.prefixDefinition, isLeftDelimited: isLeftDelimited, isRightDelimitedFunc: isRightDelimitedFunc)
                let isValidInfixOperatorName = self.isValidOperatorName(matchInfo.infixDefinition, isLeftDelimited: isLeftDelimited, isRightDelimitedFunc: isRightDelimitedFunc)
                if DEBUG {print("NEW OP `\(value)`, isLeftDelimited=\(isLeftDelimited): `\(matchInfo)`, isValidName: \(isValidPrefixOperatorName)/\(isValidInfixOperatorName)")}
                partialMatches.append(PartialOperatorMatch<T>(precedingWords: precedingWords,
                                                              currentPartialWord: currentPartialWord,
                                                              isLeftDelimited: isLeftDelimited,
                                                              startIndex: startIndex, endIndex: endIndex, info: matchInfo))
                if DEBUG {print("Got a new operator match: \(matchInfo)")}
                
                
            }
        }
        // check each current partial operator match; if a full valid operator name is matched, proceed to next stage; otherwise update or discard each partial match depending on whether or not it continues to match on current word
        var partialMatchIndex = 0
        while partialMatchIndex < partialMatches.count { // partial matches are ordered from earliest to latest; goal is to find the earliest, longest full match
            let match = partialMatches[partialMatchIndex]
            // check if we've found a complete, valid (i.e. correctly delimited) operator name
            let isValidPrefixOperatorName = self.isValidOperatorName(match.info.prefixDefinition, isLeftDelimited: match.isLeftDelimited, isRightDelimitedFunc: isRightDelimitedFunc)
            let isValidInfixOperatorName = self.isValidOperatorName(match.info.infixDefinition, isLeftDelimited: match.isLeftDelimited, isRightDelimitedFunc: isRightDelimitedFunc)
            // TO DO: fullMatch needs to indicate which name(s) are valid; when final best match is made, only include the op(s) that are valid (i.e. different op names, including aliases, can have different delimiting)
            if DEBUG {print("isValidOp: `\(match.info)`: \(isValidPrefixOperatorName), \(isValidInfixOperatorName)")}
            if isValidPrefixOperatorName || isValidInfixOperatorName { // found a full, correctly delimited operator name
                fullMatch = (match, (isValidPrefixOperatorName, isValidInfixOperatorName))
                partialMatches = [match]
                partialMatchIndex = 0
                if DEBUG {print("FOUND A FULL MATCH: \(fullMatch!)\n")}
            }
            // update current partial matches
            if DEBUG {print("MATCHING NEXT \(T.self): `\(value)` in `\(match.info.nextWords.map{$0.0})`")}
            if let nextMatch = match.info.nextWords[value] {
                partialMatches[partialMatchIndex] = PartialOperatorMatch<T>(precedingWords: match.precedingWords,
                                                                            currentPartialWord: match.currentPartialWord,
                                                                            isLeftDelimited: match.isLeftDelimited,
                                                                            startIndex: match.startIndex, endIndex: endIndex, info: nextMatch)
                if DEBUG {print("... and matched it too: \(partialMatches[partialMatchIndex])")}
                partialMatchIndex += 1
            } else {
                if DEBUG {print("...and failed to make a match, so discarding partial.")}
                partialMatches.removeAtIndex(partialMatchIndex)
            }
        }
        if let (match, type) = fullMatch {
            if match.info.isLongest || partialMatches.count == 0 { // check if longest full match has been made
                var nameToken: Token? = nil
                if match.precedingWords.count > 0 || match.currentPartialWord?.chars.count > 0 {
                    var words = match.precedingWords
                    if match.currentPartialWord?.chars.count > 0 {
                        words.append((String(match.currentPartialWord!.chars), match.currentPartialWord!.startIndex, endIndex)) // TO DO: confirm these indexes are right
                    }
                    nameToken = Token(type: .UnquotedName, value: self.joinWords(words), range: self.wordsRange(words))
                }
                //print(precedingWords)
                if DEBUG {print("FULLY MATCHED \(T.self) OPERATOR: <\(match.info.name)>     range=\(match.startIndex..<match.endIndex)")}
                return (nameToken, Token(type: .Operator, value: match.info.name!, range: match.startIndex..<match.endIndex,
                                                          operatorDefinitions: (type.prefix ? match.info.prefixDefinition : nil,
                                                                                type.infix  ? match.info.infixDefinition  : nil)))
            }
        }
        return nil
    }
    
    
    /**********************************************************************/
     // Public interface
    
    
    var currentToken: Token { return self.currentTokensCache[self.currentTokenIndex] } // current token
    
    // TO DO: FIX!!!!! ignoreVocabulary will screw up cache if it changes; safe solution is to write all tokens to cache with a flag that indicates the ignore settings with which they were originally read; advance, etc. can then check the token has the same ignore settings as it has, and flush it and the remaining tokens if not
    
    func advance(ignoreVocabulary ignoreVocabulary: Bool = false) {
        self.currentTokenIndex += 1
        if self.currentTokenIndex == self.currentTokensCache.count { self.readNextToken(ignoreVocabulary) }
        //            if self.currentTokenIndex > 100 { print("BUG: punc.advance() is stuck: "+(self.currentTokensCache.map{String($0)}.joinWithSeparator("\n\t"))); break} // TO DO: DEBUG; DELETE
    }
    
    func skip(tokenType: TokenType) throws { // advance to next token, throwing SyntaxError if it's not the specified type
        self.advance()
        if self.currentToken.type != tokenType {
            throw SyntaxError(description: "[0] Expected \(tokenType) but found \(self.currentToken.type)")
        }
    }
    
    func backtrackTo(tokenIndex: Int, flush: Bool = false) { // backtrack to a previous token, optionally flushing subsequent tokens if they need to be regenerated (e.g. with different ignoreVocabulary option)
        self.currentTokenIndex = tokenIndex
        if flush {
            self.currentTokensCache.removeRange(tokenIndex+1..<self.currentTokensCache.count)
            self.cursor = self.currentToken.range.endIndex
        }
    }
    
    
    // TO DO: CAUTION: lookahead will screw up cache if ignoreVocabulary changes, as tokens appended to cache read with one ignore setup need to be thrown out if read with another; currently this shouldn't be a problem since only parseRecord does this, and it backtracks and flushes before re-reading with different ignoreVocabulary setting
    
    func lookaheadBy(offset: UInt, ignoreVocabulary: Bool = false) -> Token { // TO DO: what about annotations? (should prob. also ignore those by default, but need to confirm)
        if offset == 0 { return self.currentToken }
        var count: UInt = 0
        var lookaheadTokenIndex: Int = self.currentTokenIndex
        //if DEBUG {print("LOOKING AHEAD from \(self.currentToken) by \(offset)")}
        while count < offset { // TO DO: .WhiteSpace tokens are no longer emitted, so this loop can be simplified
            lookaheadTokenIndex += 1
            while lookaheadTokenIndex >= self.currentTokensCache.count { self.readNextToken(ignoreVocabulary) }
            let lookaheadToken = self.currentTokensCache[lookaheadTokenIndex]
            if lookaheadToken.type == .EndOfCode {
                if DEBUG {print("LOOKAHEAD REACHED END: \(self.currentTokensCache.map{$0.value as String!})")}
                return lookaheadToken
            }
            count += 1
        }
        //       print("LOOKED AHEAD to    \(self.currentTokensCache[lookaheadTokenIndex])")
        return self.currentTokensCache[lookaheadTokenIndex]
    }
    
    func flush() { // clear cache of fully-parsed tokens (at minimum, this leaves the current token at index 0)
        self.currentTokensCache.removeRange(0..<self.currentTokenIndex)
        self.currentTokenIndex = 0
    }
}








