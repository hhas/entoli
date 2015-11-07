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

class Lexer {
    
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
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    let code: ScriptChars
    var cursor: ScriptIndex
    let codeLength: ScriptIndex
    
    // TO DO: how would/should lexer tie into incremental parsing support? would the editor, given knowledge of the AST (which in turn lets it determine which token user is currently modifying), simply re-lex after each edit? also, bear in mind the need to lex as code is edited (which means lexer needs to be able to suspend and resume on input stream, which this implementation - which uses String, not stream - currently doesn't allow)
    init(code: String) { // TO DO: option to indicate already inside quoted text/name [1] or annotation
        self.code = code.characters
        self.cursor = self.code.startIndex
        self.codeLength = self.code.endIndex
    }
    
    
    var currentToken: Token? { return nil } // current token

    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    /*
    // read and return next token; returns nil once all tokens have been read
    private func _next() {
        if self.cursor >= self.codeLength { return } // TO DO: what to do if overrun? (e.g. endlessly adding null tokens prob. isn't good idea if something gets stuck in infinite loop)
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
            // check for numeric word
            
        
        } else {
                self._readUnquotedText()
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
        print("FOUND BUG IN LEXER._NEXT()") // TO DO: should never get this far
        return self.currentTokensCache.append(gEndOfCodeToken)
    }
    
    func _readUnquotedText() {
        // note: backtracking here means using source character indexes; this is best when opportunistically parsing record items, where parser should look first for unquoted name followed by colon (a `literal-name:any-value` pair), then backtracking, switching vocab mode and retrying if the colon isn't found (one could even argue for implementing that particular search as a custom `currentTokenAsRecordItemName` method on lexer that returns either the unquoted name token or nil, avoiding need to expose another backtracking API)
        
        // other backtracking uses: opportunistic parsing of command argument, where an unsuitable token is left as-is to be read on next pass (i.e. no sense in throwing that work away); if parseAtom finds an operator that turns out to be infix (in which case it needs to backtrack in case it's reading argument to command, in which case infix op will use command name as leftExpr -- although won't parseArgument do that anyway?)
        let start = self.cursor
        while self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor]) {
            self.cursor = self.cursor.successor()
        }
        if self.cursor > start {
            let range = start..<self.cursor
            return self.currentTokensCache.append(Token(type: .UnquotedWord, value: String(self.code[range]), range: range))
        }
    }
    
    
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    
    
    private var currentTokensCache: [Token] = [gEndOfCodeToken]
    
    //
    
    private(set) var currentTokenIndex = 0 // parser can get this value and use it to backtrack to that token later on if required; caution: flush() invalidates any previously obtained indexes
    
    var currentToken: Token? { return self.currentTokensCache[self.currentTokenIndex] } // current token
    
    func advance(ignoreWhiteSpace: Bool = true) {
        repeat {
            self.currentTokenIndex += 1
            if self.currentTokenIndex == self.currentTokensCache.count { self._next() }
            if self.currentTokenIndex > 20 { print("BUG: punc.advance() is stuck: \(self.currentTokensCache.last)"); break}
        } while ignoreWhiteSpace && self.currentToken?.type == .WhiteSpace
    }
    
    func skip(tokenType: TokenType, ignoreWhiteSpace: Bool = true) throws { // advance to next token, throwing SyntaxError if it's not the specified type
        self.advance(ignoreWhiteSpace)
        if self.currentToken?.type != tokenType {
            throw SyntaxError(description: "[0] Expected \(tokenType) but found \(self.currentToken?.type)")
        }
    }
    
    func backtrackTo(tokenIndex: Int) { // note: technically this doesn't backtrack but rather moves to a previously read token (thus it could also be used to advance over previously parsed tokens for which cached Values have already been generated); might be an idea to rename it, or else replace with [safe] setter for currentTokenIndex
        self.currentTokenIndex = tokenIndex
    }
    
    // caution: lookahead doesn't know about operators/data detectors; it can only look for punctuation, whitespace, quoted name/text, and unquoted word
    func lookaheadBy(offset: UInt, ignoreWhiteSpace: Bool = true) -> Token? { // TO DO: what about annotations? (should prob. also ignore those by default, but need to confirm)
        if offset == 0 { return self.currentToken }
        var count: UInt = 0
        var lookaheadTokenIndex: Int = self.currentTokenIndex
        //if DEBUG {print("LOOKING AHEAD from \(self.currentToken) by \(offset)")}
        while count < offset {
            lookaheadTokenIndex += 1
            while lookaheadTokenIndex >= self.currentTokensCache.count { self._next() }
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
    
    func flush() { // clear cache of fully-parsed tokens
        self.currentTokensCache.removeRange(0..<self.currentTokenIndex)
        self.currentTokenIndex = 0
    }*/
}


