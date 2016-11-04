//
//  operator-tables.swift
//  entoli
//
//


// note: given need to load operator tables during parsing, it makes sense for non-trivial 'scripts' to be defined as a folder (probably with same filename sequence as single text file), each of which is parsed using whatever operators are specified by file's top-level imports, but all of which are evaluated in same namespace


// caution: symbol-based operator names, unlike other names, are case-sensitive; not sure if this should be a 'feature', though given that symbol names should never contain alphanumerics hopefully it'll end up being a non-issue anyway

// TO DO: what about symbols such as `π` (`pi`) or `∅` (`empty set`)? these would need to be defined as atoms, not commands, to ensure they don't try to take next token as argument (their long ASCII names might be defined as either atomic phrase, or left as commands that throw error if given an argument - and assuming they're available during editing could give a warning, or autocorrect, even then).


// TO DO: think we need a hard rule for devs: never implement new types using command- or prefixop-based constructors (e.g. `file "/"`), as the same name can't then be used for typespec (e.g. `"/" as file`) [technically, through proc overloading and optional args `file` probably _could_ do both, but it'd likely get nasty; avoiding it also provides clear distinction between object specifiers and datatypes, e.g. `document "foo" of ...` vs `document`, although that still requires thought]

// TO DO: how much smarts should be dedicated to detecting potential spelling errors, e.g.:
// `A is before or same B` -- correctly spelled operator parses as intended, i.e. `(A) is before or same (B)`
// `A is before or eqaal B` -- but with typo, this would give `(A) is before ('or eqaal' B)` (or throw error if both `is before` and `or` are always matched as operators) [unless we treat `or` as op with missing LH operand, in which case syntax error; but it's unclear if/how often we should do that rather than just accept it as a name---although if it's a self-delimiting op then shouldn't this always force it to be treated as op, not just in valid cases but invalid ones too]


// TO DO: should `no value`, `did nothing` be defined as Atom ops or commands? (Atoms would be safer in that these _never_ take an arg; not sure if they should self-delimit though, and given they're really only intended as return values it's not very useful if they did, so probably best use .None)

// note: entering unicode operators (e.g. for set operations) is a client-side problem; rather than defining phrase-based aliases for all [non-ASCII] symbolic operators, implement input assistance in editor where a Cmd-key press switches text entry to mnemonic input mode that allows user to type some or all of unicode character's standard name, fuzzy matching it against names of known operator symbols a-la auto-completion (basically a streamlined, inline variation on the standard Emoji+Symbols palette, without the need to muck with actual palettes); TBH, such a fuzzy auto-suggest+complete mechanism could probably be generalized for all commands and operators (might want to review Raskin)


// TO DO: `do EXPR using {NAME:VALUE,...}` operator for binding free names in expression and evaling it? Note: probably best way to allow proc objects to be manipulated as values is to use `as expression` operator, e.g. `my func: (uppercase of standard library) as expression`, though that still leaves questions on how best to invoke it and pass arguments. (It is also a little fuzzy on the meaning of "expression", since here the expression is the reference to the proc object, not the proc object itself. Furthermore, `uppercase as expression` would be expected to work the same. However, both are really capturing the command, not the proc name, and thus not the proc object itself; i.e. `uppercase` would be evaluated as an expression on use, and immediately fail because the command lacks the required argument. Thus it should probably be `as procedure`, to make sure it understands all we're giving it is the name/location of the proc object.) Simply writing `my func "hello"` would work if eval automatically treats all proc object lookups as immediately callable, thus any command name that isn't explicitly cast to expression would lookup and invoke the named procedure. FWIW, this fits with entoli's "say what you want" philosophy, which strongly favors [explicit] instruction-directed behavior rather than [implicit] type-directed behavior (with all the user assumptions about invisible - and largely runtime-specific - information that entails). One particular downside of this usage pattern: `my func` will not be usefully introspectable until the `uppercase of standard library` reference is resolved, which means it is much less useful to editor (at least until it can be evaluated, at which point the editor could even offer to add in signature info in form of an additional cast, or replacing the existing `as procedure` cast with a more precise one: `as procedure {SIGNATURE}`)

// Q. could `EXPR as procedure {SIG}` usefully convert a single/group expression value to a procedure? (this'd certainly be the preferred method, given that entoli strongly favors casts over constructors in order to avoid double-purposed names [i.e. homonyms] as commonly found in AppleScript, e.g. `TEXT as file` vs `file TEXT`)




//**********************************************************************
// standard operator argument names

// note: matching ops defined as both prefix and infix/postfix (e.g. `-`, which is both prefix 'negation' and infix 'subtraction' operators) depends on proc overloading, which in turn relies on receiving a record arg with unambiguous keys (and explicitly named keys are as unambiguous as it gets)
// (note that overloading the `-` symbol would normally be considered a misuse of overloading in that its behaviors are not simply variations on the same theme but quite different, e.g. negation vs subtraction, but as this overloading is defined by arithmetic itself we have to accept in this case)

let gLeftOperandKeyString  = "left"
let gRightOperandKeyString = "right"

let gLeftOperandName = Name(gLeftOperandKeyString)
let gRightOperandName = Name(gRightOperandKeyString)

let gAsOperatorKeyString = "as"
let gAsOperatorPrecedence = 80

//**********************************************************************



extension Command { // convenience constructors used by operator parsefuncs
    
    convenience init(_ name: String, leftOperand: Value) {
        self.init(name, Pair(gLeftOperandName, leftOperand))
    }
    convenience init(_ name: String, leftOperand: Value, rightOperand: Value) {
        self.init(name, Pair(gLeftOperandName, leftOperand), Pair(gRightOperandName, rightOperand))
    }
    convenience init(_ name: String, rightOperand: Value) {
        self.init(name, Pair(gRightOperandName, rightOperand))
    }
}


// standard prefix/infix/postfix operator parsing functions

// caution: postfix ops MUST label their operand `gNameRight` to avoid any possible confusion with prefix ops of the same name, as they cannot be distinguished by number of arguments alone (the above convenience constructors will label all operands automatically and are recommended for constructing commands for all unary and binary operators)

func parseAtomOperator(_ parser: Parser, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName)
}

func parsePrefixOperator(_ parser: Parser, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName, leftOperand: try parser.parseExpression(precedence))
}

func parseInfixOperator(_ parser: Parser, leftExpr: Value, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName, leftOperand: leftExpr, rightOperand: try parser.parseExpression(precedence))
}

func parseRightInfixOperator(_ parser: Parser, leftExpr: Value, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName, leftOperand: leftExpr, rightOperand: try parser.parseExpression(precedence-1))
}

func parsePostfixOperator(_ parser: Parser, leftExpr: Value, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName, rightOperand: leftExpr)
}

func parseMisplacedToken(_ parser: Parser, operatorName: String, precedence: Int) throws -> Value {
    throw SyntaxError(description: "Found misplaced \(parser.lexer.currentToken.type) token: \(parser.lexer.currentToken.value)") // TO DO: should probably throw sub-error here, and leave parser to construct full error message and throw as SyntaxError
}
func parseMisplacedToken(_ parser: Parser, leftExpr: Value, operatorName: String, precedence: Int) throws -> Value {
    throw SyntaxError(description: "Found misplaced \(parser.lexer.currentToken.type) token: \(parser.lexer.currentToken.value)") // TO DO: should probably throw sub-error here, and leave parser to construct full error message and throw as SyntaxError
}



//**********************************************************************
// Operator matching tables


struct OperatorPart<ElementType: Hashable>: CustomStringConvertible { // ElementType is String or Character // TO DO: struct or class?
    // An operator name is composed of one or more 'words'. In a keyword-based operator, e.g. "is not equal to", each word is a String, created by splitting the full name on interstitial whitespace. (In a symbol-based operator, e.g. "!=", the full name should always be a single String-based word.) The PunctuationLexer outputs single String-based words, e.g. ["is", "not", "same", "as"], so to match an entire operator, each defined operator name is first broken down into nested dictionaries, each of whose keys are a single 'word' to match, and whose values are the next matchable word[s] (if any) and/or an operator definition (if a full match has been made).
    // In addition, any 'words' not identified as operator names defined in the PhraseOperatorsTable need to be examined character-by-character to see if they contain any symbol-based operators, e.g. "foo<bar". (Ideally, users would always surround symbol operators with whitespace, making them trivial to identify, but this is not an ideal world so we must check for cases where a user might accidentally/deliberately enter symbol operators without explicitly separating them from adjoining characters; a task further complicated by the fact that some characters take on different meanings according to immediate context, e.g. `-` might be a negation or subtraction operator, or an in-word hyphen.)
    
    typealias Element = ElementType
    
    typealias WordsDictionary = [ElementType:OperatorPart<ElementType>]
    
    // note: if prefix/infix definition != nil, a valid match has been made; however, if isLongest is false, there might still be a longer match to be made, in which case keep looking
    
    var prefixDefinition: PrefixOperatorDefinition? = nil
    var infixDefinition:  InfixOperatorDefinition? = nil
    
    var nextWords: [ElementType:OperatorPart<ElementType>] = [:]
    var isLongest: Bool { return self.nextWords.count == 0 }
    
    var name: String? { return self.prefixDefinition?.name.text ?? self.infixDefinition?.name.text }
    
    var description: String {return "<OperatorPart prefixOp=\(self.prefixDefinition?.name) infixOp=\(self.infixDefinition?.name) next=\(Array(self.nextWords.keys))>"}
    
    mutating func addDefinition(_ definition: OperatorDefinition) throws {
        switch definition.parseFunc {
        case .atom(let parseFunc), .prefix(let parseFunc):
            if self.prefixDefinition != nil { throw SyntaxError(description: "Duplicate operator definition: \(definition)") } // error type?
            self.prefixDefinition = (definition.name, definition.precedence, parseFunc, definition.aliases)
        case .infix(let parseFunc), .postfix(let parseFunc):
            if self.infixDefinition != nil { throw SyntaxError(description: "Duplicate operator definition: \(definition)") } // error type?
            self.infixDefinition = (definition.name, definition.precedence, parseFunc, definition.aliases)
        }
    }
}


// lookup table for Keyword (e.g. "is not equal to") or Symbol ("!=") operators
class OperatorTable<ElementType: Hashable> { // Keyword/Symbol table (only real difference is that first matches on words, the second on chars)
    // given a multi-word operator name, split it into words, then store as a series of nested dicts, ending in the operator definition itself
    
    typealias Part = OperatorPart<ElementType>
    
    var definitionsByPart: Part.WordsDictionary = [:]
    
    func _addOperator(_ words: [ElementType], wordsTable: inout Part.WordsDictionary, definition: OperatorDefinition) {
        var words = words
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
    
    func addOperator(_ name: String, definition: OperatorDefinition) {
        self._addOperator(Array(name.characters), wordsTable: &self.definitionsByPart, definition: definition)
    }
}


class PhraseOperatorsTable: OperatorTable<String> { // whole-word matching
    
    func addOperator(_ name: String, definition: OperatorDefinition) {
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
    
    
    func add(_ definition: OperatorDefinition) -> Self {
        // given a multi-word operator name, split it into words, then store as a series of nested dicts,
        // ending in the operator definition itself
        for name in [definition.name] + definition.aliases {
            if name.type == .symbol {
                self.symbols.addOperator(name.text, definition: definition)
            } else {
                self.phrases.addOperator(name.text, definition: definition)
            }
        }
        return self
    }
    
    func add(_ definitions: [OperatorDefinition]) -> Self {
        for definition in definitions { let _ = self.add(definition) }
        return self
    }
}


