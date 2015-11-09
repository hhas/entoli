//
//  operator-tables.swift
//  entoli
//
//


// TO DO: think we need a hard rule for devs: never implement new types using command- or prefixop-based constructors (e.g. `file "/"`), as the same name can't then be used for typespec (e.g. `"/" as file`) [technically, through proc overloading and optional args `file` probably _could_ do both, but it'd likely get nasty; avoiding it also provides clear distinction between object specifiers and datatypes, e.g. `document "foo" of ...` vs `document`, although that still requires thought]


// TO DO: how to disambiguate overloaded op/command names? e.g. `'-'{1}` (neg) might be distinguished from `'-'{2,1}` (sub) by counting args, but that won't work where a prefix op overloads a postfix op; also, in the `-` case, `'-'{1,2}` would be a more correct representation (`-1` and `2-1` both negate the RH operand, not the left, but record items count left to right so the required value wants to be first, followed by the optional one), but then that causes more confusion (plus, of course, there's also `2;'-'1` to take into account, and even `1;'-'`)

// TO DO: how much smarts should be dedicated to detecting potential spelling errors, e.g.:
// `A is before or same B` -- correctly spelled operator parses as intended, i.e. `(A) is before or same (B)`
// `A is before or eqaal B` -- but with typo, this would give `(A) is before ('or eqaal' B)` (or throw error if both `is before` and `or` are always matched as operators) [unless we treat `or` as op with missing LH operand, in which case syntax error; but it's unclear if/how often we should do that rather than just accept it as a name---although if it's a self-delimiting op then shouldn't this always force it to be treated as op, not just in valid cases but invalid ones too]

// note: `-100` and `-foo` are both valid uses of prefix `-`; however, it would be better that first is matched as number before trying to match it as symbol operator, so perhaps it's just a question of doing keyword op parse first, then pattern-based value detection, then in-word symbol operator detection? Or maybe we should even just say whitespace or other delimiter char is always required around symbol ops? (FWIW, this should happen anyway: e.g. `- 1` will be parsed as prefix op, while `-1` will be parsed as word, with longest-match rule automatically ensuring it's read as a number)


// TO DO: should `no value`, `did nothing` be defined as Atom ops or commands? (Atoms would be safer in that these _never_ take an arg; not sure if they should self-delimit though, and given they're really only intended as return values it's not very useful if they did, so probably best use .None)



// standard prefix/infix/postfix operator parsing functions // TO DO: how to distinguish pre/in/post arguments when command name is same? use explicit property labels in record? (e.g. even standardizing on generic `{first value:..., second value:...}`) would do it)

func parseAtomOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    return CommandValue(name: NameValue(data: operatorName), data: RecordValue(data: []))
}

func parsePrefixOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    return CommandValue(name: NameValue(data: operatorName), data: RecordValue(data: [try parser.parseExpression(precedence)]))
}

func parseInfixOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    return CommandValue(name: NameValue(data: operatorName), data: RecordValue(data: [leftExpr, try parser.parseExpression(precedence)]))
}

func parseRightInfixOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    return CommandValue(name: NameValue(data: operatorName), data: RecordValue(data: [leftExpr, try parser.parseExpression(precedence-1)]))
}

func parsePostfixOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    return CommandValue(name: NameValue(data: operatorName), data: RecordValue(data: [leftExpr]))
}


func parseGeneralComparisonOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    // TO DO: if next token is UnquotedWord "as", parse that as type to which both operands should be cast before comparing (i.e. append it to RecordValue `as:typespec` item)
    let rightOperand = try parser.parseExpression(precedence)
    return CommandValue(name: NameValue(data: operatorName), data: RecordValue(data: [leftExpr, rightOperand]))
}


func parseAtomDoBlock(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    // TO DO: need to parse expression group up to `done` UnquotedWord, returning ExpressionGroup (possibly tagged to indicate display preference)
    return CommandValue(name: NameValue(data: operatorName), data: RecordValue(data: [try parser.parseExpression(precedence)]))
}

func parsePostfixDoBlock(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    // TO DO: ditto, then attach it to leftExpr (how best to do this? and should it only apply to commands [including names], or would the be uses in attaching it to other things as well?)
    return CommandValue(name: NameValue(data: operatorName), data: RecordValue(data: [leftExpr]))
}




/*
// TO DO: when parsing param record, `to` operator should check result itself to ensure all items are name and/or name:typespec pair literals [with or without annotations attached] (or should typed params be written as `name AS typespec` ops? The `to` [define procedure] command could accept either, so it's basically a question of which users find easier; suspect colon-pairs, but need to decide.)

    func parseProcedureDefinition(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value { // returns CommandValue
        switch token.type {
        case .QuotedName: // '...' // an explicitly delimited name literal
            // if next significant token is another expr, treat it as an RH operand (i.e. argument) and emit CommandValue, else return NameValue
            return try parseArgument(NameValue(data: token.value))
        case .UnquotedWord: // everything else that is not one of the above hardcoded token types; parseWord will deal with this
            return try parseArgument(try self.parseUnquotedWord(), precedence: precedence)
        default:
            throw SyntaxError("Malformed command signature.")
        }
        if self.lookahead(1) == PunctuationLexer.TokenType.RecordLiteral {
            arg = self.parseAtom()
            // need to check record is correctly structured: each item must be a name, optionally wrapped in coercion (don't forget optional annotations too - these will be treated as arg descriptions) // TO DO: is this right/optimal?... actually, no, it's the command's record that needs to be checked, as if it has pairs then LH must be name
        }

    }
*/




func throwMisplacedToken(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    throw SyntaxError(description: "Found misplaced \(parser.currentToken?.type) token: \(parser.currentToken?.value)") // TO DO: should probably throw sub-error here, and leave parser to construct full error message and throw as SyntaxError
}



// Standard operators

// TO DO: should operators be grouped by category (math, etc) where practical, allowing them to be selectively loaded/unloaded? (that alternative would be to define each operator on corresponding proc, but don't think that level of granularity will make code any easier to follow); or is it enough just to define operator table within library, and leave library client to indicate whether or not it wants to import library's operator definitions in addition to its procs

let StandardOperators: [OperatorDefinition] = [ // .Symbol operators will be detected using both whole-word AND in-word (by-char) matching; .Keyword operators by whole-word only
    
    // TO DO: is it worth including user-viewable metadata (e.g. short descriptions) here, or is it best to leave all such metadata for commands to provide? (i.e. is there any metadata that a command wouldn't have but an operator should?)
    
    // TO DO: disambiguating `-` (neg op/sub op/in-word hyphen) is going to be problematic
    
    // TO DO: also consider `-n`; this is presumably intended to negate the value of `n` but as written refers to slot named "-n" instead (changing it to right-auto-delimit might fix this, but need to check)
    
    // arithmetic
    (("^", .Symbol, .Full),   500, .Infix,  parseRightInfixOperator, []), // TO DO: what to use as exponent operator? (`^`, `exp`?)
    (("+", .Symbol, .None),   490, .Prefix, parsePrefixOperator,     []),
    (("-", .Symbol, .None),   490, .Prefix, parsePrefixOperator,     [("–", .Symbol, .None)]), // note: also accepts n-dash as synonym // TO DO: not 100% decided about this, as n- and m-dashes could arguably be used in unquoted text to mean themselves
    (("×", .Symbol, .Full),   480, .Infix,  parseInfixOperator,      [("*", .Symbol, .Full)]),
    (("÷", .Symbol, .Full),   480, .Infix,  parseInfixOperator,      [("/", .Symbol, .Full)]),
    (("+", .Symbol, .None),   470, .Infix,  parseInfixOperator,      []),
    (("-", .Symbol, .None),   470, .Infix,  parseInfixOperator,      [("–", .Symbol, .None)]), // note: also accepts n-dash as synonym // TO DO: subtraction and hyphenation symbol is same, so adjoining whitespace is required to distinguish the two
    (("div", .Phrase, .Full), 480, .Infix,  parseInfixOperator,      []), // TO DO: allow `//` as alias?
    (("mod", .Phrase, .Full), 480, .Infix,  parseInfixOperator,      []), // TO DO: allow whitespace-delimited `%` as alias? (note: `%` is a unit suffix)
    
    // numeric comparisons (non-ASCII symbols have ASCII aliases for alternative input)
    ((">", .Symbol, .Full),   400, .Infix,  parseInfixOperator, []),
    (("<", .Symbol, .Full),   400, .Infix,  parseInfixOperator, []),
    (("=", .Symbol, .Full),   400, .Infix,  parseInfixOperator, [("==", .Symbol, .Full)]),
    (("≠", .Symbol, .Full),   400, .Infix,  parseInfixOperator, [("!=", .Symbol, .Full)]),
    (("≤", .Symbol, .Full),   400, .Infix,  parseInfixOperator, [("<=", .Symbol, .Full)]),
    (("≥", .Symbol, .Full),   400, .Infix,  parseInfixOperator, [(">=", .Symbol, .Full)]),
    
    // concatenation
    (("&", .Symbol, .Full),   450, .Infix,  parseInfixOperator, []),

    
    // non-numeric comparisons (e.g. text); note that parsefunc overrides standard precedence to allow `COMP as TYPE` to specify comparison type, e.g. `A is B as C` is shorthand for `(A as C) is (B as C)` (currently, `as` operator binds lowest, so would apply to comparison's result, but since these ops always return boolean that isn't really useful, whereas applying cast to both operands prior to comparison is, and allows things like case-insensitive text comparisons and list of X comparisons to be done as well) -- note that a failed cast will throw error (not sure if catching this should be done by typespec, and if it is then what's to prevent `A is not B as C` returning true when both A and B fail to cast causing typespec to supply identical default value for each)
    // TO DO: might be best to use `equals` rather than `is`, since `is` is likely to be used in boolean value names; also prob. best to avoid `as` at end of name, as that will get confused with `as` clause
    (("is before",         .Phrase, .Full), 400, .Infix, parseGeneralComparisonOperator, [("lt",                   .Phrase, .Full)]),
    (("is same or before", .Phrase, .Full), 400, .Infix, parseGeneralComparisonOperator, [("le",                   .Phrase, .Full),
                                                                                          ("is same as or before", .Phrase, .Full),
                                                                                          ("is before or same as", .Phrase, .Full),
                                                                                          ("is not after",         .Phrase, .Full)]),
    (("is after",          .Phrase, .Full), 400, .Infix, parseGeneralComparisonOperator, [("gt",                   .Phrase, .Full)]),
    (("is same or after",  .Phrase, .Full), 400, .Infix, parseGeneralComparisonOperator, [("ge",                   .Phrase, .Full),
                                                                                          ("is same as or after",  .Phrase, .Full),
                                                                                          ("is after or same as",  .Phrase, .Full),
                                                                                          ("is not before",        .Phrase, .Full)]),
    (("is not",            .Phrase, .Full), 400, .Infix, parseGeneralComparisonOperator, [("ne",                   .Phrase, .Full),
                                                                                          ("is not same as",       .Phrase, .Full)]),
    (("is",                .Phrase, .Full), 400, .Infix, parseGeneralComparisonOperator, [("eq",                   .Phrase, .Full),
                                                                                          ("is same as",           .Phrase, .Full)]),
    
    // Boolean
    (("not",     .Phrase, .Right), 100, .Prefix,  parsePrefixOperator, []),
    (("and",     .Phrase,  .Full),  98, .Infix,   parseInfixOperator,  []),
    (("xor",     .Phrase,  .Full),  96, .Infix,   parseInfixOperator,  []),
    (("or",      .Phrase,  .Full),  94, .Infix,   parseInfixOperator,  []),
    
    // cast
    (("as",      .Phrase,  .Full), 80, .Infix,    parseInfixOperator,  []),
    
    // reference
    (("of",       .Phrase, .Full), 800, .Infix,   parseInfixOperator,  []), // TO DO: what precedence?
    (("thru",     .Phrase, .Full),  50, .Infix,   parseInfixOperator,  [("through", .Phrase, .Full)]), // range constructor // TO DO: optional `by` clause?
    (("where",    .Phrase, .Full),  50, .Infix,   parseInfixOperator,  [("whose", .Phrase, .Full)]),  // filter clause
    
    // eval clauses
    (("catching", .Phrase, .Full),  50, .Infix,   parseInfixOperator,  []), // evaluate LH operand; on error, evaluate RH operand
    (("else",     .Phrase, .Full),  50, .Infix,   parseInfixOperator,  []), // evaluate LH operand; if it returns 'did nothing', evaluate RH operand
    
    // expression blocks
    (("do",       .Phrase, .Full),  50, .Atom,    parseAtomDoBlock,    []),
    (("do",       .Phrase, .Full),  50, .Postfix, parsePostfixDoBlock, []),
    (("done",     .Phrase, .Full),   0, .Atom,    throwMisplacedToken, []), // `do` block parsefuncs use `done` as terminator; anywhere else is a syntax error (note: this won't work if parsing per-line; TO DO: would be better to use same approach as for punctuation-based blocks where imbalances are counted [note: the latter currently don't do this either, but there's a TODO for that too])
    
    // TO DO: `to` operator for defining new procs
]



struct OperatorPart<ElementType: Hashable>: CustomStringConvertible { // ElementType is String or Character // TO DO: struct or class?
    // An operator name is composed of one or more 'words'. In a keyword-based operator, e.g. "is not same as", each word is a String, created by splitting the full name on interstitial whitespace. (In a symbol-based operator, e.g. "!=", the full name should always be a single String-based word.) The PunctuationLexer outputs single String-based words, e.g. ["is", "not", "same", "as"], so to match an entire operator, each defined operator name is first broken down into nested dictionaries, each of whose keys are a single 'word' to match, and whose values are the next matchable word[s] (if any) and/or an operator definition (if a full match has been made).
    // In addition, any 'words' not identified as operator names defined in the PhraseOperatorsTable need to be examined character-by-character to see if they contain any symbol-based operators, e.g. "foo<bar". (Ideally, users would always surround symbol operators with whitespace, making them trivial to identify, but this is not an ideal world so we must check for cases where a user might accidentally/deliberately enter symbol operators without explicitly separating them from adjoining characters; a task further complicated by the fact that some characters take on different meanings according to immediate context, e.g. `-` might be a negation or subtraction operator, or an in-word hyphen.)
    
    typealias Element = ElementType
    
    typealias WordsDictionary = [ElementType:OperatorPart<ElementType>]
    
    // note: if prefix/infix definition != nil, a valid match has been made; however, if isLongest is false, there might still be a longer match to be made, in which case keep looking
    
    var prefixDefinition: OperatorDefinition? = nil
    var infixDefinition:  OperatorDefinition? = nil
    
    var nextWords: [ElementType:OperatorPart<ElementType>] = [:]
    var isLongest: Bool { return self.nextWords.count == 0 }
    
    var name: String? { return self.prefixDefinition?.name.text ?? self.infixDefinition?.name.text }
    
    var description: String {return "<OperatorPart prefixOp=\(self.prefixDefinition?.name) infixOp=\(self.infixDefinition?.name) next=\(Array(self.nextWords.keys))>"}
    
    mutating func addDefinition(definition: OperatorDefinition) throws {
        if definition.form.hasLeftOperand {
            if self.infixDefinition != nil { throw SyntaxError(description: "Duplicate operator definition: \(definition)") } // error type?
            self.infixDefinition = definition
        } else {
            if self.prefixDefinition != nil { throw SyntaxError(description: "Duplicate operator definition: \(definition)") } // error type?
            self.prefixDefinition = definition
        }
    }
}


// lookup table for Keyword (e.g. "is not same as") or Symbol ("!=") operators
class OperatorTable<ElementType: Hashable> { // Keyword/Symbol table (only real difference is that first matches on words, the second on chars)
    // given a multi-word operator name, split it into words, then store as a series of nested dicts, ending in the operator definition itself
    
    typealias Part = OperatorPart<ElementType>
    
    private(set) var definitionsByPart: Part.WordsDictionary = [:]
    
    private func _addOperator(var words: [ElementType], inout wordsTable: Part.WordsDictionary, definition: OperatorDefinition) {
        if definition.precedence % 2 != 0 { // note: right association relies on subtracting 1 from normal precedence
            print("Operator has non-even precedence: \(definition)") // TO DO: how best to deal with this? throw?
        }
        let word = words.removeFirst()
        if wordsTable[word] == nil {
            wordsTable[word] = OperatorPart<ElementType>()
        }
        if words.count > 0 {
            self._addOperator(words, wordsTable: &(wordsTable[word]!.nextWords), definition: definition)
        } else {
            do {
                try wordsTable[word]!.addDefinition(definition)
            } catch {
                print("Can't define operator: \(error)") // looks like an operator with the same name and *fix has already been added // TO DO: how best to deal with this? throw?
            }
        }
    }
}




class SymbolOperatorsTable: OperatorTable<Character> {
    
    func addOperator(name: String, definition: OperatorDefinition) {
        self._addOperator(Array(name.characters), wordsTable: &self.definitionsByPart, definition: definition)
    }
}


class PhraseOperatorsTable: OperatorTable<String> { // whole-word matching
    
    func addOperator(name: String, definition: OperatorDefinition) {
        // TO DO: what about normalizing name? (trim, lowercase, etc), or is it reasonable to expect tables to be correctly formatted before reading?
        self._addOperator(name.characters.split{$0 == " "}.map(String.init), wordsTable: &self.definitionsByPart, definition: definition)
    }
}


//


class Operators {
    
    typealias Phrase = PhraseOperatorsTable.Part
    typealias Symbol = SymbolOperatorsTable.Part
    
    let phrases = PhraseOperatorsTable()
    let symbols = SymbolOperatorsTable()
    
    
    func add(definition: OperatorDefinition) -> Self {
        // given a multi-word operator name, split it into words, then store as a series of nested dicts,
        // ending in the operator definition itself
        for name in [definition.name] + definition.aliases {
            if name.type == .Symbol {
                self.symbols.addOperator(name.text, definition: definition)
            } else {
                self.phrases.addOperator(name.text, definition: definition)
            }
        }
        return self
    }
    
    func add(definitions: [OperatorDefinition]) -> Self {
        for definition in definitions { self.add(definition) }
        return self
    }
}



let StandardOperatorsTable = Operators().add(StandardOperators)



