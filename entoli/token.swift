//
//  token.swift
//  entoli
//
//


//**********************************************************************
// operator definition


enum OperatorType {
    case phrase
    case symbol
}

enum OperatorForm { // TO DO: distinguish keyword from symbol
    case atom
    case prefix
    case infix
    case postfix
    
    var hasLeftOperand: Bool { return (self == .infix || self == .postfix) }
}


enum AutoDelimit { // e.g. Given word sequence `red is blue`, should it be parsed as a single name, or as an `is` operator with `red` and `blue` operands?
    case Left
    case Right
    case full // `red is blue` will be parsed as operation; to make it a single name, user must single-quote it: `'red is blue'`
    case none // `red is blue` will be parsed as single name; to make it an operation, user must punctuate it: `'red' is 'blue'`, `(red) is (blue)`, etc.
    
    // If `left` and/or `right` property is true, this op does not require its left and/or right operand to be explicitly delimited.
    //
    // e.g. Assume `A` and `B` are UnquotedWord tokens, and `(A/B)` is any explicitly delimited operand (group/list/quoted/etc literal):
    //
    // - if .Full, `A op B` *always* parses to operation `(A) op (B)`
    // - if .Left, `op B` parses to `op (B)`, but `A op` parses to name 'A op'
    // - if .None, *only* `(A) op (B)` parses to operation `(A) op (B)`
    //
    var left:  Bool { return (self == .Left  || self == .full) }
    var right: Bool { return (self == .Right || self == .full) }
}


typealias ParseFuncType = (Parser, leftExpr: Value?, operatorName: String, precedence: Int) throws -> Value

typealias OperatorName = (text: String, type: OperatorType, autoDelimit: AutoDelimit)

typealias OperatorDefinition = (name: OperatorName, precedence: Int, form: OperatorForm, parseFunc: ParseFuncType, aliases: [OperatorName])

typealias OperatorDefinitions = (prefixDefinition: OperatorDefinition?, infixDefinition: OperatorDefinition?)


//**********************************************************************
// token


let gOperatorDefinedPrecedence = -2


enum TokenType { // TO DO: implement human-readable names for use in error messages
    case startOfCode
    case endOfCode
    // Punctuation
    case quotedText // atomic; the lexer automatically reads everything between `"` and corresponding `"`, including `""` escapes
    case quotedName // atomic; the lexer automatically reads everything between `'` and corresponding `'`, including `''` escapes
    case annotationLiteral // atomic; the lexer automatically reads everything between `«` and corresponding `»`, including nested annotations
    case annotationLiteralEnd // this will only appear in token stream if not balanced by an earlier .AnnotationLiteral
    case listLiteral // an ordered collection (array) or key-value collection (dictionary)
    case listLiteralEnd
    case recordLiteral // a sequence of values and/or name-value pairs; primarily used to represent complex procedure arguments
    case recordLiteralEnd
    case expressionSequenceLiteral
    case expressionSequenceLiteralEnd
    case expressionSeparator
    case itemSeparator
    case pairSeparator
    case pipeSeparator
    case lineBreak
    case unquotedWord // everything else that is not one of the above predefined token types // TO DO: get rid of this once vocab lexing is done
    // Vocabulary (note: Lexer converts unquoted words and related tokens to the following token types, according to hardcoded rules and lookup tables)
    case numericWord // atomic; a word that represents a whole/decimal/hexadecimal number, optionally including exponent and/or unit type prefix/suffix
    case `operator` // atomic/prefix/infix/postfix; a recognized operator (basically syntactic sugar for a standard command), including its definition
    case unquotedName // atomic, or special-case prefix; equivalent to QuotedName in function, but constructed from contiguous sequence of non-special unquoted words
    
    var precedence: Int {
        switch self {
        case .annotationLiteral:    return 1000
        case .expressionSeparator:  return 50
        case .itemSeparator:        return 50
        case .pairSeparator:        return 60
        case .pipeSeparator:        return 5000 // TO DO: confirm this, as it creates a non-trivial transform, e.g. `foo; bar + 1` -> `bar{foo} + 1`
        case .operator:             return gOperatorDefinedPrecedence
        default:                    return 0
        }
    }
}


struct Token: CustomStringConvertible {
    let type: TokenType
    let value: String // normalized representation
    let range: ScriptRange // position of original (raw) input in source code
    let partial: Int // >0 = missing N close tokens; <0 = missing N open tokens
    
    // one or both of the following are non-nil when token type is .Operator
    let prefixOperator: OperatorDefinition?
    let infixOperator:  OperatorDefinition?
    // the following is non-nil when token type is .NumericWord
    let numericInfo:   Numeric?
    
    init(type: TokenType, value: String, range: ScriptRange, partial: Int = 0,
            operatorDefinitions: OperatorDefinitions = (nil, nil), numericInfo: Numeric? = nil) {
        self.type = type
        self.value = value
        self.range = range
        self.partial = partial
        (self.prefixOperator, self.infixOperator) = operatorDefinitions
        self.numericInfo = numericInfo
    }
    
    var description: String {
        return "«\(self.type) \(self.range) `\(self.value)`»" // TO DO: extended representations for numerics and operators
    }
}


private let emptyRange = Range(uncheckedBounds: (lower: "".startIndex, upper: "".startIndex))


let gStartOfCodeToken = Token(type: .startOfCode, value: "««STARTCODE»»", range: emptyRange) // null token, used in cachedTokens to indicate start of script
let gEndOfCodeToken = Token(type: .endOfCode, value: "««ENDOFCODE»»", range: emptyRange) // null token, used in cachedTokens to indicate end of script



