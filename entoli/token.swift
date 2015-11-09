//
//  token.swift
//  entoli
//
//


/**********************************************************************/
// operator definition


enum OperatorType {
    case Phrase
    case Symbol
}

enum OperatorForm { // TO DO: distinguish keyword from symbol
    case Atom
    case Prefix
    case Infix
    case Postfix
    
    var hasLeftOperand: Bool { return (self == .Infix || self == .Postfix) }
}


enum AutoDelimit { // e.g. Given word sequence `red is blue`, should it be parsed as a single name, or as an `is` operator with `red` and `blue` operands?
    case Left
    case Right
    case Full // `red is blue` will be parsed as operation; to make it a single name, user must single-quote it: `'red is blue'`
    case None // `red is blue` will be parsed as single name; to make it an operation, user must punctuate it: `'red' is 'blue'`, `(red) is (blue)`, etc.
    
    // If `left` and/or `right` property is true, this op does not require its left and/or right operand to be explicitly delimited.
    //
    // e.g. Assume `A` and `B` are UnquotedWord tokens, and `(A/B)` is any explicitly delimited operand (group/list/quoted/etc literal):
    //
    // - if .Full, `A op B` *always* parses to operation `(A) op (B)`
    // - if .Left, `op B` parses to `op (B)`, but `A op` parses to name 'A op'
    // - if .None, *only* `(A) op (B)` parses to operation `(A) op (B)`
    //
    var left:  Bool { return (self == .Left  || self == .Full) }
    var right: Bool { return (self == .Right || self == .Full) }
}


typealias ParseFuncType = (Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value

typealias OperatorName = (text: String, type: OperatorType, autoDelimit: AutoDelimit)

typealias OperatorDefinition = (name: OperatorName, precedence: Int, form: OperatorForm, parseFunc: ParseFuncType, aliases: [OperatorName])

typealias OperatorDefinitions = (prefixDefinition: OperatorDefinition?, infixDefinition: OperatorDefinition?)


/**********************************************************************/
// token

typealias ScriptChars = String.CharacterView
typealias ScriptIndex = ScriptChars.Index
typealias ScriptRange = Range<ScriptIndex> // position of this token within the original source code (note: this may be different size to Token.value due to white space and operator name normalization)


let gOperatorDefinedPrecedence = -2


enum TokenType { // TO DO: implement human-readable names for use in error messages
    case StartOfCode
    case EndOfCode
    // Punctuation
    case WhiteSpace
    case QuotedText // atomic; the lexer automatically reads everything between `"` and corresponding `"`, including `""` escapes
    case QuotedName // atomic; the lexer automatically reads everything between `'` and corresponding `'`, including `''` escapes
    case AnnotationLiteral // atomic; the lexer automatically reads everything between `«` and corresponding `»`, including nested annotations
    case AnnotationLiteralEnd // this will only appear in token stream if not balanced by an earlier .AnnotationLiteral
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
    case UnquotedWord // everything else that is not one of the above predefined token types // TO DO: get rid of this once vocab lexing is done
    // Vocabulary (note: Lexer converts unquoted words and related tokens to the following token types, according to hardcoded rules and lookup tables)
    case NumericWord // atomic; a word that represents a whole/decimal/hexadecimal number, optionally including exponent and/or unit type prefix/suffix
    case Operator // atomic/prefix/infix/postfix; a recognized operator (basically syntactic sugar for a standard command), including its definition
    case UnquotedName // atomic, or special-case prefix; equivalent to QuotedName in function, but constructed from contiguous sequence of non-special unquoted words
    
    var precedence: Int {
        switch self {
        case .AnnotationLiteral:    return 1000
        case .ExpressionSeparator:  return 50
        case .ItemSeparator:        return 50
        case .PairSeparator:        return 60
        case .PipeSeparator:        return 50
        case .Operator:             return gOperatorDefinedPrecedence
        default:                    return 0
        }
    }
}


struct Token {
    let type: TokenType
    let value: String // normalized representation
    let range: ScriptRange // position of original (raw) input in source code
    let partial: Int // >0 = missing N close tokens; <0 = missing N open tokens
    
    // one or both of the following are non-nil when token type is .Operator
    let prefixOperator: OperatorDefinition?
    let infixOperator:  OperatorDefinition?
    // the following is non-nil when token type is .NumericWord
    let numericInfo:   NumericValue?
    
    init(type: TokenType, value: String, range: ScriptRange, partial: Int = 0,
            operatorDefinitions: OperatorDefinitions = (nil, nil), numericInfo: NumericValue? = nil) {
        self.type = type
        self.value = value
        self.range = range
        self.partial = partial
        (self.prefixOperator, self.infixOperator) = operatorDefinitions
        self.numericInfo = numericInfo
    }
}



let gStartOfCodeToken = Token(type: .StartOfCode, value: "", range: "".startIndex..<"".endIndex) // null token, used in cachedTokens to indicate start of script
let gEndOfCodeToken = Token(type: .EndOfCode, value: "", range: "".startIndex..<"".endIndex) // null token, used in cachedTokens to indicate end of script



