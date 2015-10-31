//
//  lexer.swift
//  entoli
//
//  This chunks code according to hardcoded tokens that never change (literal delimiters, punctuation, and whitespace), 
//  but not unquoted text (names, commands, operators, unquoted numbers and units, etc.) which is tokenized separately 
//  from UnquotedWords and their interstitial tokens using runtime-customizable data pattern and operator tables.
//

// note: an entoli script can contain three things: code, user docs (annotations) and developer docs (comments) // TO DO: in implementation terms, can comments safely be treated as a particular type of annotation, with different code delimiters but same the syntax rules and internal storage mechanism? Or do they need more flexibility/hygene, e.g. since they're often also used temporarily to block out broken/unfinished code during development and testing? (note: the same 3-prong approach - user+dev+trace info - should apply to error reporting)

// note: quoted text literals support escaping of quotes only; for other escapes (e.g. tabs, linefeed, codepoints, etc), use `format "..."` command (this should use tags, e.g. `{tab}`, `{0u1234 0uABCD}`, `{some name}`; if not parameterized, result can also be memoized for efficiency; note that allowing commands in tags, they should be sandboxed to prevent side-effects - though this is technically 'honesty method' since there's no way to enforce this restriction on primitive procs if they choose to lie about it)


enum OperatorForm {
    case Atom
    case Prefix
    case Infix
    case Postfix
    
    var hasLeftOperand: Bool { return (self == .Infix || self == .Postfix) }
}


class Lexer : GeneratorType {
        
    enum TokenType {
        case WhiteSpace
        case QuotedText
        case QuotedName
        case AnnotationLiteral
        case AnnotationLiteralEnd
        case ListLiteral // an ordered collection (array) or key-value collection (dictionary)
        case ListLiteralEnd
        case RecordLiteral // a sequence of values and/or name-value pairs; primarily used to represent complex procedure arguments
        case RecordLiteralEnd
        case ExpressionGroupLiteral
        case ExpressionGroupLiteralEnd
        case ExpressionSeparator
        case ItemSeparator
        case PairSeparator
        case PipeSeparator
        case LineBreak
        case UnquotedWord // everything else that is not one of the above hardcoded token types; second-stage lexer/parser will deal with this
    }
    
    struct Token {
        let type: TokenType
        let value: String
        let range: Range<String.CharacterView.Index>
        let partial: Int // >0 = missing N close tokens; <0 = missing N open tokens
        
        init(type: TokenType, value: String, range: Range<String.CharacterView.Index>, partial: Int = 0) {
            self.type = type
            self.value = value
            self.range = range
            self.partial = partial
        }
    }
    
    typealias Element = Token
    
    
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
        .union(Set(punctuation.keys)).union(Set(nonBreakingWhiteSpace)) // note: command/operator tokenizer should  never overload these tokens; e.g. to read an unquoted decimal number, look for digit[s]/Lexer.ExpressionSeparator/digit[s]
    
    let code: String.CharacterView
    var cursor: String.CharacterView.Index
    let length: String.CharacterView.Index
    
    // TO DO: how would/should lexer tie into incremental parsing support? would the editor, given knowledge of the AST (which in turn lets it determine which token user is currently modifying), simply re-lex after each edit? also, bear in mind the need to lex as code is edited (which means lexer needs to be able to suspend and resume on input stream, which this implementation - which uses String, not stream - currently doesn't allow)
    init(code: String) { // TO DO: option to indicate already inside quoted text/name [1] or annotation
        self.code = code.characters
        self.cursor = self.code.startIndex
        self.length = self.code.endIndex
    }
        
    // read and return next token; returns nil once all tokens have been read
    func next() -> Element? {
        if self.cursor >= length { return nil }
        let start = self.cursor
        let firstChar = self.code[self.cursor] // TO DO: prob make this `char` var and rejig stages into a single switch
        //
        // quoted text/name
        if let token = self.dynamicType.quoteDelimiters[firstChar] { // found open quote
            var chars = String.CharacterView()
            var isquote = true
            while isquote {
                self.cursor = self.cursor.successor() // eat open quote
                let substart = self.cursor // [1] if already starting inside quote, need to initialize token, text, isquote, and start vars, skip eating open quote (since there isn't one), then continue from this line
                while self.cursor < length && self.dynamicType.quoteDelimiters[self.code[self.cursor]] != token {
                    self.cursor = self.cursor.successor()
                }
                if self.cursor == length {
                    chars.appendContentsOf(self.code[substart..<self.cursor])
                    return Token(type: token, value: String(chars), range: start..<self.cursor, partial: +1)
                }
                chars.appendContentsOf(self.code[substart..<self.cursor])
                self.cursor = self.cursor.successor() // eat close quote
                isquote = self.cursor < length && self.dynamicType.punctuation[self.code[self.cursor]] == token
                if isquote { // note: quote chars are escaped by typing twice (the first is ignored, the second used)
                    chars.append(self.code[self.cursor])
                }
            }
            return Token(type: token, value: String(chars), range: start..<self.cursor)
        }
        //
        // unquoted word (i.e. anything that isn't quoted, punctuation or white space); TO DO: would there be any value in parser supplying a function to perform second-stage lexing/parsing of this text as it arrives? (debatable: it'd need to know a bit much about lexing process here, as this lexer only reads single 'words' and isn't smart enough to deal with [e.g.] numbers containing decimal points; for now, it's simplest to kick everything to parser and let it decide at its leisure, and worry about shaving extravagant CPU cycles if/when/once it all works)
        while self.cursor < length && !self.dynamicType.reservedCharacters.contains(self.code[self.cursor]) {
            self.cursor = self.cursor.successor()
        }
        if self.cursor > start {
            let range = start..<self.cursor
            return Token(type: .UnquotedWord, value: String(self.code[range]), range: range)
        }
        //
        // 'non-breaking' white space; unlike LF, space and TAB do not delimit unquoted text but instead are treated as part of it; allowing the parser to reconstitute white space-separated words as a single token if/where/when it wishes
        //(note: the only reason for supplying code's original whitespace to parser is that it allows code editor to choose between preserving user's whitespace or replacing it with AST's canonical whitespace (normalizing whitespace during editing would be extremely annoying to user as it would cause code to change length and move around while they're still working on it; thus it is probably best that parser treat it as prefix/postfix annotations on adjoining nodes; annotations themselves being tagged and stored in a dict)
        while self.cursor < length && self.dynamicType.nonBreakingWhiteSpace.contains(self.code[self.cursor]) {
            self.cursor = self.cursor.successor()
        }
        if self.cursor > start {
            let range = start..<self.cursor
            return Token(type: .WhiteSpace, value: String(self.code[range]), range: range)
        }
        //
        // «...» annotations are atomic and nestable and read as-is (understanding their contents is left to the caller)
        if let t = self.dynamicType.annotationDelimiters[firstChar] {
            if t < 0 { // TO DO: how to handle case where `»` is encountered first?
                return Token(type: .AnnotationLiteralEnd, value: String(firstChar), range: start..<self.cursor)
            }
            let start = self.cursor
            var level = 1
            while level > 0 {
                self.cursor = self.cursor.successor()
                if self.cursor == length {
                    let range = start..<self.cursor
                    return Token(type: .AnnotationLiteral, value: String(self.code[range]), range: range, partial: level)
                }
                level += self.dynamicType.annotationDelimiters[self.code[self.cursor]] ?? 0
            }
            self.cursor = self.cursor.successor()
            let range = start..<self.cursor
            return Token(type: .AnnotationLiteral, value: String(self.code[range]), range: range)
        }
        //
        // all other tokens (single-char)
        if let token = self.dynamicType.punctuation[firstChar] {
            //               let pos = self.cursor
            self.cursor = self.cursor.successor()
            return Token(type: token, value: String(firstChar), range: start..<self.cursor)
        }
        print("UH-OH") // TO DO: should never get this far
        return nil
    }
}


