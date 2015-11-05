//
//  lexer.swift
//  entoli
//
//
//  Combines punctuation and vocabulary lexers behind a common interface, switching between them as required.
//


private let DEBUG = false

class Lexer {
    
    private let punctuationLexer: PunctuationLexer
    private let vocabularyLexer: VocabularyLexer
    
    init(code: String, keywordOperators: OperatorPhrasesTable) {
        self.punctuationLexer = PunctuationLexer(code: code)
        self.vocabularyLexer = VocabularyLexer(lexer: self.punctuationLexer, keywordOperators: keywordOperators)
    }
    
    //

    typealias CachedToken = (token: Token?, ignoreVocabulary: Bool, isVocabulary: Bool, tokenIndex: Int) // isVocabulary determines which lexer produced this token; use tokenIndex to backtrack that lexer to that token
    
    private var currentTokensCache: [CachedToken?] = [nil]
    
    private(set) var currentTokenIndex = 0
    
    var currentToken: Token? { return self.currentTokensCache[self.currentTokenIndex]?.token } // current token
    
    var isReadingVocabulary: Bool = false // TO DO: private?
    
    func _readVocabulary(ignoreVocabulary: Bool, punctuationTokenBacktrackIndex: Int) -> CachedToken {
        if DEBUG {print("START WORD SEQ ON \(self.punctuationLexer.currentToken) (current lexer token=\(self.currentToken))")}
        if ignoreVocabulary { // read entire sequence as single unquoted name value
            let firstWord = self.punctuationLexer.currentToken!
            var words: [String] = [firstWord.value]
            while (self.punctuationLexer.lookaheadBy(1, ignoreWhiteSpace: false)?.type == .WhiteSpace
                && self.punctuationLexer.lookaheadBy(2, ignoreWhiteSpace: false)?.type == .UnquotedWord) {
                    self.punctuationLexer.advance()
                    words.append(self.punctuationLexer.currentToken!.value)
            }
            let token = Token(type: .UnquotedName, value: words.joinWithSeparator(" "),
                range: firstWord.range.startIndex..<self.punctuationLexer.currentToken!.range.endIndex)
            return (token, ignoreVocabulary, self.isReadingVocabulary, punctuationTokenBacktrackIndex)
        } else {
            if DEBUG {print("START VOCAB")}
            self.isReadingVocabulary = true
            self.vocabularyLexer.advance()
            let token = self.vocabularyLexer.currentToken
            if token?.type == .UnquotedWord { print("BUG[5]: lexer adding unquoted word: \(token)") }
            if token == nil {print("\nBUG: started reading vocab, but got nil token\n")} // TO DO: check vocab lexer never returns nil when reading a UnquotedWord
            return (token, ignoreVocabulary, self.isReadingVocabulary, punctuationTokenBacktrackIndex)
        }
    }
    
    func _next(ignoreWhiteSpace: Bool, ignoreVocabulary: Bool) { // advance `currentToken` to next punctuation/vocabulary token
        self.punctuationLexer.advance(ignoreWhiteSpace && !isReadingVocabulary) // TO DO: confirm this doesn't screw up vocab parsing (might need)
        let punctuationTokenBacktrackIndex = self.punctuationLexer.currentTokenIndex // Lexer.backtrackTo uses this to backtrack the punctuation lexer
        if self.isReadingVocabulary { // currently reading vocab
            if ignoreVocabulary { // this shouldn't happen unless parser is being silly/awkward/buggy (parser should decide ignoreVocabulary _before_ proceeding to read an unquoted word sequence; e.g. when reading a record item, it should start with ignoreVocabulary=true and attempt to parse the item as a NAME:VALUE pair, then backtrack and reparse with ignoreVocabulary=false if it doesn't find one)
                print("BUG: currently reading vocab, but ignoreVocabulary is true")
                self.currentTokensCache.append(nil)
                return
            }
            self.vocabularyLexer.advance()
            if let token = self.vocabularyLexer.currentToken { // read next vocab token from vocab lexer and store here
                if DEBUG {print("READ VOCAB TOKEN:", token)}
                if token.type == .UnquotedWord { print("BUG[1]: lexer adding unquoted word: \(token)") }
                self.currentTokensCache.append((token, ignoreVocabulary, self.isReadingVocabulary, punctuationTokenBacktrackIndex))
            } else if let token = self.punctuationLexer.currentToken { // ...no more vocab tokens left in vocab lexer (either they've all been consumed, or were flushed), so return to processing punctuation
                if DEBUG {print("END VOCAB; READ PUNC TOKEN:", token)}
                self.isReadingVocabulary = false
                self.vocabularyLexer.flush()
                var nextToken: Token? = token
                while ignoreWhiteSpace && token.type == .WhiteSpace {
                    if DEBUG {print("...ignoring whitespace and advancing to next token...")}
                    self.punctuationLexer.advance() // TO DO: confirm this can't advance onto another vocab
                    nextToken = self.punctuationLexer.currentToken
                }
                if DEBUG {print(self.currentToken == nil ? "END OF CODE" : "... READ PUNC TOKEN: \(self.currentToken)")}
                
                if nextToken?.type == .UnquotedWord {
                    print("Lexer resuming vocabularizing unquoted word: \(token)")
                    let cachedToken = self._readVocabulary(ignoreVocabulary, punctuationTokenBacktrackIndex: punctuationTokenBacktrackIndex)
                    self.currentTokensCache.append(cachedToken)
                } else {
                    self.currentTokensCache.append((nextToken, ignoreVocabulary, self.isReadingVocabulary, punctuationTokenBacktrackIndex))
                }
            } else {
                if DEBUG {print("END OF CODE")}
                self.currentTokensCache.append(nil)
            }
        } else if self.punctuationLexer.currentToken?.type == .UnquotedWord { // found start of an unquoted word sequence
            let cachedToken = self._readVocabulary(ignoreVocabulary, punctuationTokenBacktrackIndex: punctuationTokenBacktrackIndex)
            self.currentTokensCache.append(cachedToken)
        } else {
            let token = self.punctuationLexer.currentToken
            if token?.type == .UnquotedWord { print("BUG[6]: lexer adding unquoted word: \(token)") }
            self.currentTokensCache.append((token, ignoreVocabulary, self.isReadingVocabulary, punctuationTokenBacktrackIndex))
        }
    }
    



    
    func advance(ignoreWhiteSpace: Bool = true, ignoreVocabulary: Bool = false) {
        repeat {
            self.currentTokenIndex += 1
            if self.currentTokenIndex == self.currentTokensCache.count { self._next(ignoreWhiteSpace, ignoreVocabulary: ignoreVocabulary) }
        } while ignoreWhiteSpace && self.currentToken?.type == .WhiteSpace
    }
    
    func skip(tokenType: TokenType, ignoreWhiteSpace: Bool = true, ignoreVocabulary: Bool = false) throws { // advance to next token, throwing SyntaxError if it's not the specified type
        self.advance(ignoreWhiteSpace, ignoreVocabulary: ignoreVocabulary)
        if self.currentToken?.type != tokenType {
            throw SyntaxError(description: "[0] Expected \(tokenType) but found \(self.currentToken?.type)")
        }
    }
    
    func backtrackTo(tokenIndex: Int) {
        // TO DO: need to confirm backtracking into/over/out of multiple vocab blocks doesn't screw up lexers' integrity
        
        print("BACKTRACKING from \(self.currentToken) @\(self.currentTokenIndex) TO \(tokenIndex)")
        
        self.currentTokenIndex = tokenIndex
        self.currentTokensCache.removeRange(tokenIndex+1..<self.currentTokensCache.count)
        // flushing vocab is a bit crude (the alternative would be to create and cache a new vocab lexer instance for each unquoted word sequence, but there's no point adding that complexity until/unless it's clearly needed)
        
        
        // TO DO: this isn't working right; trying to backtrack from `!=` to `bob` causes `!=` token to be tossed (which isn't ideal in itself), but puts us back on `!=` token in punc lexer, which means next advance puts us past it
        self.isReadingVocabulary = false
        self.vocabularyLexer.flush()
        
        guard let cachedToken = self.currentTokensCache[tokenIndex] else {
            print("BUG: backtracked to cached token \(tokenIndex) but found only nil. All subsequent behavior is wrong.")
            return
        }
        self.punctuationLexer.backtrackTo(cachedToken.tokenIndex)
        print("... and punc lexer backtracked to: \(self.punctuationLexer.currentToken)")
    }
    
    // caution: lookahead doesn't know about operators/data detectors; it can only look for punctuation, whitespace, quoted name/text, and unquoted word
    func lookaheadBy(offset: UInt, ignoreWhiteSpace: Bool = true, ignoreVocabulary: Bool = false) -> Token? { // TO DO: what about annotations? (should prob. also ignore those by default, but need to confirm)
        // TO DO: need to confirm looking ahead into/over/out of multiple vocab blocks doesn't screw up lexers' integrity
        if offset == 0 { return self.currentToken }
        var count: UInt = 0
        var lookaheadTokenIndex: Int = self.currentTokenIndex
        print("[START] lookaheadBy(\(offset),\(ignoreWhiteSpace),\(ignoreVocabulary))   CACHE COUNT=",self.currentTokensCache.count, "CURRENT INDEX=", self.currentTokenIndex)
        for (i,o) in self.currentTokensCache.enumerate() {print("\t\(i).\t",o)}
        if DEBUG || true {print("LEXER LOOKING AHEAD from \(self.currentToken) at \(self.currentTokenIndex) by \(offset)")}
        while count < offset {
            lookaheadTokenIndex += 1
            while lookaheadTokenIndex >= self.currentTokensCache.count { self._next(ignoreWhiteSpace, ignoreVocabulary: ignoreVocabulary) }
            guard let lookaheadToken = self.currentTokensCache[lookaheadTokenIndex] else {
                if DEBUG {print("LEXER LOOKAHEAD REACHED END: \(self.currentTokensCache.map{$0?.token?.value as String!})")}
                return nil
            }
            guard let token = lookaheadToken.token else {
                if DEBUG {print("LEXER LOOKAHEAD REACHED END: \(self.currentTokensCache.map{$0?.token?.value as String!})")}
                return nil
            }
            if token.type != .WhiteSpace || !ignoreWhiteSpace { count += 1 }
        }
        print("[ENDED] lookaheadBy   CACHE COUNT=",self.currentTokensCache.count, "CURRENT INDEX=", self.currentTokenIndex)
        for (i,o) in self.currentTokensCache.enumerate() {print("\t\(i).\t",o)}
        print("LEXER LOOKED AHEAD to    \(self.currentTokensCache[lookaheadTokenIndex])")
        return self.currentTokensCache[lookaheadTokenIndex]?.token
    }
    
    
    func flush() { // clear cache of fully-parsed tokens
        self.isReadingVocabulary = false
        self.vocabularyLexer.flush()
        self.punctuationLexer.flush()
        self.currentTokensCache = [nil]
        self.currentTokenIndex = 0
    }
}