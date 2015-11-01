//
//  operator-tables.swift
//  entoli
//
//

// TO DO: auto-delimiting rules are different for whole-word vs in-word ops (for symbol ops, whitespace is also a delimiter, so should always fully self-delimit when found in keyword table, which currently they won't do; one option is to make OperatorDefinition a class with KeywordOperatorDefinition and SymbolOperatorDefinition subclasses, or add a .Keyword/.Symbol flag and maybe group type:form:delimit: into a sub-tuple for convenience; alternatively, use separate keywordDelimit: and symbolDelimit:)


// TO DO: think we need a hard rule for devs: never implement new types using command- or prefixop-based constructors (e.g. `file "/"`), as the same name can't then be used for typespec (e.g. `"/" as file`) [technically, through proc overloading and optional args `file` probably _could_ do both, but it'd likely get nasty; avoiding it also provides clear distinction between object specifiers and datatypes, e.g. `document "foo" of ...` vs `document`, although that still requires thought]


// TO DO: how to disambiguate overloaded op/command names? e.g. `'-'{1}` (neg) might be distinguished from `'-'{2,1}` (sub) by counting args, but that won't work where a prefix op overloads a postfix op; also, in the `-` case, `'-'{1,2}` would be a more correct representation (`-1` and `2-1` both negate the RH operand, not the left, but record items count left to right so the required value wants to be first, followed by the optional one), but then that causes more confusion (plus, of course, there's also `2;'-'1` to take into account, and even `1;'-'`)

// TO DO: how much smarts should be dedicated to detecting potential spelling errors, e.g.:
// `A is before or equal B` -- correctly spelled operator parses as intended, i.e. `(A) is before or equal (B)`
// `A is before or eqaal B` -- but with typo, this would give `(A) is before ('or eqaal' B)` (or throw error if both `is before` and `or` are always matched as operators) [unless we treat `or` as op with missing LH operand, in which case syntax error; but it's unclear if/how often we should do that rather than just accept it as a name---although if it's a self-delimiting op then shouldn't this always force it to be treated as op, not just in valid cases but invalid ones too]

// note: `-100` and `-foo` are both valid uses of prefix `-`; however, it would be better that first is matched as number before trying to match it as symbol operator, so perhaps it's just a question of doing keyword op parse first, then pattern-based value detection, then in-word symbol operator detection? Or maybe we should even just say whitespace or other delimiter char is always required around symbol ops? (FWIW, this should happen anyway: e.g. `- 1` will be parsed as prefix op, while `-1` will be parsed as word, with longest-match rule automatically ensuring it's read as a number)


// TO DO: should `no value`, `did nothing` be defined as Atom ops or commands? (Atoms would be safer in that these _never_ take an arg; not sure if they should self-delimit though, and given they're really only intended as return values it's not very useful if they did, so probably best use .None)


enum OperatorForm { // TO DO: distinguish keyword from symbol
    case Atom
    case Prefix
    case Infix
    case Postfix
    
    var hasLeftOperand: Bool { return (self == .Infix || self == .Postfix) }
    var isSymbol: Bool { return false } // TO DO
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

typealias OperatorDefinition = (name: String, aliases: [String], precedence: Int, form: OperatorForm, autoDelimit: AutoDelimit, parseFunc: ParseFuncType)


// standard prefix/infix/postfix operator parsing functions // TO DO: how to distinguish pre/in/post arguments when command name is same? use explicit property labels in record? (e.g. even standardizing on generic `{first value:..., second value:...}`) would do it)

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
        if self.lookahead(1) == Lexer.TokenType.RecordLiteral {
            arg = self.parseAtom()
            // need to check record is correctly structured: each item must be a name, optionally wrapped in coercion (don't forget optional annotations too - these will be treated as arg descriptions) // TO DO: is this right/optimal?... actually, no, it's the command's record that needs to be checked, as if it has pairs then LH must be name
        }

    }
*/




func throwMisplacedToken(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    throw SyntaxError(description: "Found misplaced \(parser.currentToken?.type) token: \(parser.currentToken?.value)") // TO DO: should probably throw sub-error here, and leave parser to construct full error message and throw as SyntaxError
}


// TO DO: might be cleanest to use operator form (.Infix, etc) to distinguish symbol from keyword; this also eliminates need for separate tables, and allows op tables to be combined (symboltable.add would be called by keywordtable.add)

// Standard operators

let StandardSymbolOperators: [OperatorDefinition] = [ // these will be detected using both whole-word AND in-word (by-char) matching
    
    // TO DO: is it worth including user-viewable metadata (e.g. short descriptions) here, or is it best to leave all such metadata for commands to provide? (i.e. is there any metadata that a command wouldn't have but an operator should?)
    
    // TO DO: disambiguating `-` (neg op/sub op/in-word hyphen) is going to be problematic
    
    // arithmetic
    ("^", [   ], 500, .Infix,  .Full, parseRightInfixOperator), // TO DO: what to use as exponent operator? (`^`, `exp`?)
    ("+", [   ], 490, .Prefix, .Full, parsePrefixOperator),
    ("-", ["–"], 490, .Prefix, .None, parsePrefixOperator),     // note: also accepts n-dash as synonym
    ("*", [   ], 480, .Infix,  .Full, parseInfixOperator),
    ("/", [   ], 480, .Infix,  .Full, parseInfixOperator),
    ("+", [   ], 470, .Infix,  .Full, parseInfixOperator),
    ("-", ["–"], 470, .Infix,  .None, parseInfixOperator),      // note: also accepts n-dash as synonym // TO DO: subtraction and hyphenation symbol is same, so adjoinng whitespace is required to distinguish the two
    
    // numeric comparisons (non-ASCII symbols have ASCII aliases for alternative input)
    (">", [    ], 400, .Infix, .Full, parseInfixOperator),
    ("<", [    ], 400, .Infix, .Full, parseInfixOperator),
    ("=", ["=="], 400, .Infix, .Full, parseInfixOperator),
    ("≠", ["!="], 400, .Infix, .Full, parseInfixOperator),
    ("≤", ["<="], 400, .Infix, .Full, parseInfixOperator),
    ("≥", [">="], 400, .Infix, .Full, parseInfixOperator),
    
    // concatenation
    ("&", [], 450, .Infix, .Full, parseInfixOperator),
] // note: symbol ops should be added to both keyword (for speedy whole-word lookup) and symbol op (for in-word lookup) tables

// note: there currently isn't a convenient way to provide keyword aliases for symbol names, and vice-versa

let StandardKeywordOperators: [OperatorDefinition] = [
    // arithmetic
    ("div", [], 480, .Infix, .Full, parseInfixOperator),
    ("mod", [], 480, .Infix, .Full, parseInfixOperator),
    
    // non-numeric comparisons (e.g. text); note that parsefunc overrides standard precedence to allow `COMP as TYPE` to specify comparison type, e.g. `A is B as C` is shorthand for `(A as C) is (B as C)` (currently, `as` operator binds lowest, so would apply to comparison's result, but since these ops always return boolean that isn't really useful, whereas applying cast to both operands prior to comparison is, and allows things like case-insensitive text comparisons and list of X comparisons to be done as well) -- note that a failed cast will throw error (not sure if catching this should be done by typespec, and if it is then what's to prevent `A is not B as C` returning true when both A and B fail to cast causing typespec to supply identical default value for each)
    // TO DO: what about `eq`, `lt`, etc. shorthand aliases for these? should `is [same [as]]` be accepted? (toss-up between keeping it forgiving and learnable, though bear in mind that all aliases should automatically convert to canonical form when pretty-printed - they are purely for input convenience, not to riddle users' scripts with synonyms); would `is same or after`, etc. work better than `is equal or after` (in which case change them all to use 'same', not 'equal')?
    // TO DO: these operators need custom parsefunc that looks for trailing `as` operator and grabs that too as the type (if any) to which both operands should be cast _before_ comparing them to each other (basically these non-numeric comparison ops are a multifix op where the last operand is optional, allowing complex comparisons - text, date, lists of <whatever>, case-insensitive text [since typespecs can also supply normalized values/comparator funcs for use in comparisons and sorting], and so on)
    ("is before",          ["lt",                                                                  ], 400, .Infix, .Full, parseGeneralComparisonOperator),
    ("is equal or before", ["le", "is equal to or before", "is before or equal to", "is not after" ], 400, .Infix, .Full, parseGeneralComparisonOperator),
    ("is after",           ["gt",                                                                  ], 400, .Infix, .Full, parseGeneralComparisonOperator),
    ("is equal or after",  ["ge", "is equal to or after",  "is after or equal to",  "is not before"], 400, .Infix, .Full, parseGeneralComparisonOperator),
    ("is not",             ["ne", "is not equal to",                                               ], 400, .Infix, .Full, parseGeneralComparisonOperator),
    ("is",                 ["eq", "is equal to",                                                   ], 400, .Infix, .Full, parseGeneralComparisonOperator),
    
    // Boolean
    // TO DO: should AND, OR, and NOT be pretty-printed in [small]caps? If so, how/where best to define custom formatting hints? (Note: while the quick answer would be here in ops table, this is really part of a much larger, more general question as to how formatting should be applied to ALL code, e.g. according to high-level semantics such as categories of procedures and data relationships rather than superficial syntactic structure. For example, given an obvious list literal, is it not more useful for highlighting to indicate where it came from or what it is used for, rather than just reiterate 'bracket,string,comma,string,comma,...,bracket'? This suggests presentation metadata should be attached to command definitions, via standard annotations mechanism, and introspectable by parser as it reads the code. One implication of this: all module dependencies would need to be declared - and readable+resolvable - at parse-time, not runtime; or, at least, all those that wish to define metadata, or supply additional presentation recommendations for commands used in user's code; ditto for providing auto-assist - auto-suggest, auto-complete, auto-disambiguate - to user.)
    ("not", [], 100, .Prefix, .Right, parsePrefixOperator),
    ("and", [],  99, .Infix,  .Full,  parseInfixOperator),
    ("or",  [],  98, .Infix,  .Full,  parseInfixOperator),
    
    // cast
    ("as", [], 80, .Infix, .Full, parseInfixOperator),
    
    // reference
    ("of",    [         ], 800, .Infix, .Full, parseInfixOperator), // TO DO: what precedence?
    ("thru",  ["through"],  50, .Infix, .Full, parseInfixOperator), // range constructor
    ("where", ["whose"  ],  50, .Infix, .Full, parseInfixOperator),  // filter clause
    
    // eval clauses
    ("catching", [], 50, .Infix,   .Full, parseInfixOperator),
    ("else",     [], 50, .Infix,   .Full, parseInfixOperator),
    ("do",       [], 50, .Atom,    .Full, parseAtomDoBlock),
    ("do",       [], 50, .Postfix, .Full, parsePostfixDoBlock),
    ("done",     [],  0, .Atom,    .Full, throwMisplacedToken), // `do` block parsefuncs use `done` as terminator; anywhere else is a syntax error
    
    // TO DO: `to` operator for defining new procs
]



struct OperatorWordInfo<WordT: Hashable>: CustomStringConvertible { // WordT is String or Character
    // An operator name is composed of one or more 'words'. In a keyword-based operator, e.g. "is not equal to", each word is a String, created by splitting the full name on interstitial whitespace. (In a symbol-based operator, e.g. "!=", the full name should always be a single String-based word.) The Lexer outputs single String-based words, e.g. ["is", "not", "equal", "to"], so to match an entire operator, each defined operator name is first broken down into nested dictionaries, each of whose keys are a single 'word' to match, and whose values are the next matchable word[s] (if any) and/or an operator definition (if a full match has been made).
    // In addition, any 'words' not identified as operator names defined in the KeywordOperatorTable need to be examined character-by-character to see if they contain any symbol-based operators, e.g. "foo<bar". (Ideally, users would always surround symbol operators with whitespace, making them trivial to identify, but this is not an ideal world so we must check for cases where a user might accidentally/deliberately enter symbol operators without explicitly separating them from adjoining characters; a task further complicated by the fact that some characters take on different meanings according to immediate context, e.g. `-` might be a negation or subtraction operator, or an in-word hyphen.)
    typealias WordsDictionary = [WordT:OperatorWordInfo<WordT>]
    
    // note: if definition != nil, a valid match has been made; however, if isLongest is false, 
    // there might still be a longer match to be made, in which case keep looking // TO DO: a recursive algorithm can capture the valid match in the current call frame, then check isLongest to determine if it should recurse further; it will also need a way to start the next match on whatever words were left in subframes after the longest match is made - simplest way to do that may just be for subframes to call Lexer.skip(-n) to backtrack (where n is typically 2, i.e. word+interstitial whitespace tokens)
    
    var definition: OperatorDefinition? = nil // TO DO: separate prefix and infix defs
    
    var nextWords: [WordT:OperatorWordInfo<WordT>] = [:]
    var isLongest: Bool { return self.nextWords.count == 0 }
    
    var description: String {return "<OperatorWordInfo defn=\(self.definition?.name) next=\(Array(self.nextWords.keys))>"}
}


// lookup table for Keyword (e.g. "is not equal to") or Symbol ("!=") operators
class OperatorTable<WordT: Hashable> { // Keyword/Symbol table (only real difference is that first matches on words, the second on chars)
    // given a multi-word operator name, split it into words, then store as a series of nested dicts, ending in the operator definition itself
    
    typealias WordInfoType = OperatorWordInfo<WordT>
    typealias WordsDictionary = WordInfoType.WordsDictionary
    
    private(set) var prefixDefinitions: WordsDictionary = [:] // operators without a left operand
    private(set) var infixDefinitions:  WordsDictionary = [:] // operators with a left operand
    
    private func _addOperator(var words: [WordT], inout table: WordsDictionary, definition: OperatorDefinition) {
        let word = words.removeFirst()
        if table[word] == nil {
            table[word] = OperatorWordInfo<WordT>()
        }
        if words.count > 0 {
            self._addOperator(words, table: &(table[word]!.nextWords), definition: definition)
        } else {
            if table[word]!.definition != nil {
                print("Duplicate operator entry \(word)") // looks like an operator with the same name and *fix has already been added // TO DO: how best to deal with this?
            }
            table[word]!.definition = definition
        }
    }
    
    func splitWords(name: String) -> [WordT] { // concrete subclasses override this
        return []
    }
    
    
    // TO DO: merge prefix/infix defs into single table
    
    
    // TO DO: move this to keyword subclass; it should then call symbol subclass as needed (it should prob. also check that symbols don't contain ws)
    func add(definitions: [OperatorDefinition]) -> Self {
        for definition in definitions {
            // given a multi-word operator name, split it into words, then store as a series of nested dicts, 
            // ending in the operator definition itself
            for name in [definition.name] + definition.aliases {
                // Swift's dogawful 'copy by value' plus inability to use `&` on expressions is a huge PITA
                if definition.form.hasLeftOperand {
                    self._addOperator(self.splitWords(name), table: &self.infixDefinitions, definition: definition)
                } else {
                    self._addOperator(self.splitWords(name), table: &self.prefixDefinitions, definition: definition)
                }
            }
        }
        return self
    }
}




class SymbolOperatorTable: OperatorTable<Character> {
    
    override func splitWords(name: String) -> [Character] {
        return Array(name.characters)
    }
}



class KeywordOperatorTable: OperatorTable<String> { // whole-word matching
    
    override func splitWords(name: String) -> [String] {// TO DO: what about normalizing name? (trim, lowercase, etc), or is it reasonable to expect tables to be correctly formatted before reading?
        return name.characters.split{$0 == " "}.map(String.init)
    }
}


let StandardSymbolOperatorTable = SymbolOperatorTable().add(StandardSymbolOperators)

let StandardKeywordOperatorTable = KeywordOperatorTable().add(StandardSymbolOperators).add(StandardKeywordOperators)



