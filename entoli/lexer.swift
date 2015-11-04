//
//  lexer.swift
//  entoli
//
//
//  Combine punctuation and vocabulary lexers behind common interface
//


// benefit of this over punctuation lexer managing vocab lexer is that no cached tokens are lost when backtracking and switching mode

private let DEBUG = false

class Lexer {
    
    private let punctuationLexer: PunctuationLexer
    private let vocabularyLexer: VocabularyLexer
    
    init(code: String, keywordOperators: OperatorPhrasesTable) {
        self.punctuationLexer = PunctuationLexer(code: code)
        self.vocabularyLexer = VocabularyLexer(lexer: self.punctuationLexer, keywordOperators: keywordOperators)
    }
    
    
    
    // note: unquoted word sequence can only be read one of two ways: as 1+ vocab tokens or as unquoted name token
    
    // TO DO: suspect we need to keep previous tokens for backtracking
    
    var currentToken: Token? // current token
    
    var isReadingVocabulary: Bool = false
    
    func advance(ignoreWhiteSpace: Bool = true, ignoreVocabulary: Bool = false) {
        self.punctuationLexer.advance(ignoreWhiteSpace && !isReadingVocabulary) // TO DO: confirm this doesn't screw up vocab parsing (might need)
        if self.isReadingVocabulary { // currently reading vocab
            if ignoreVocabulary {
                if DEBUG {print("BUG: currently reading vocab, but ignoreVocabulary is true")}
                return
            }
            self.vocabularyLexer.advance()
            if let nextToken = self.vocabularyLexer.currentToken {
                if DEBUG {print("READ VOCAB TOKEN:", nextToken)}
                self.currentToken = nextToken
            } else if let nextToken = self.punctuationLexer.currentToken {
                if DEBUG {print("END VOCAB; READ PUNC TOKEN:", nextToken)}
                self.isReadingVocabulary = false
                self.vocabularyLexer.flush()
                if ignoreWhiteSpace && nextToken.type == .WhiteSpace {
                    if DEBUG {print("...ignoring whitespace and advancing to next token...")}
                    self.punctuationLexer.advance() // TO DO: confirm this can't advance onto another vocab
                    self.currentToken = self.punctuationLexer.currentToken
                    if DEBUG {print(self.currentToken == nil ? "END OF CODE" : "... READ PUNC TOKEN: \(self.currentToken)")}
                } else {
                    self.currentToken = nextToken
                }
            } else {
                if DEBUG {print("END OF CODE")}
                self.currentToken = nil
            }
        } else if self.punctuationLexer.currentToken?.type == .UnquotedWord { // found start of an unquoted word sequence
            if DEBUG {print("START WORD SEQ ON \(self.punctuationLexer.currentToken) (current lexer token=\(self.currentToken))")}
            if ignoreVocabulary { // read entire sequence as single unquoted name value
                let firstWord = self.punctuationLexer.currentToken!
                var words: [String] = [firstWord.value]
                while (self.punctuationLexer.lookaheadBy(1, ignoreWhiteSpace: false)?.type == .WhiteSpace
                        && self.punctuationLexer.lookaheadBy(2, ignoreWhiteSpace: false)?.type == .UnquotedWord) {
                    self.punctuationLexer.advance()
                    words.append(self.punctuationLexer.currentToken!.value)
                }
                self.currentToken = Token(type: .UnquotedName, value: words.joinWithSeparator(" "),
                                          range: firstWord.range.startIndex..<self.punctuationLexer.currentToken!.range.endIndex)
            } else {
                if DEBUG {print("START VOCAB")}
                self.isReadingVocabulary = true
                self.vocabularyLexer.advance()
                self.currentToken = self.vocabularyLexer.currentToken
                if self.currentToken == nil {print("\nBUG: started reading vocab, but got nil token\n")} // TO DO: check vocab lexer never returns nil when reading a UnquotedWord
            }
        } else {
            self.currentToken = self.punctuationLexer.currentToken
        }
    }
    
    
    // TO DO: implement following
    
    var currentTokenIndex: Int { return self.punctuationLexer.currentTokenIndex }
    
    
    func skip(tokenType: TokenType, ignoreWhiteSpace: Bool = true, ignoreVocabulary: Bool = false) throws { // advance to next token, throwing SyntaxError if it's not the specified type
        self.advance(ignoreWhiteSpace, ignoreVocabulary: ignoreVocabulary)
        if self.currentToken?.type != tokenType {
            throw SyntaxError(description: "[0] Expected \(tokenType) but found \(self.currentToken?.type)")
        }
    }
    
    func backtrackTo(tokenIndex: Int) {
        self.punctuationLexer.backtrackTo(tokenIndex)
    }
    
    // caution: lookahead doesn't know about operators/data detectors; it can only look for punctuation, whitespace, quoted name/text, and unquoted word
    func lookaheadBy(offset: UInt, ignoreWhiteSpace: Bool = true, ignoreVocabulary: Bool = false) -> Token? { // TO DO: what about annotations? (should prob. also ignore those by default, but need to confirm)
        
        
        return nil
        
    }
    
    
    func flush() { // clear cache of fully-parsed tokens
        self.currentToken = nil
        self.isReadingVocabulary = false
        self.punctuationLexer.flush()
        self.vocabularyLexer.flush()

    }
}