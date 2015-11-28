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


let gLeftOperandKeyString  = "left"
let gRightOperandKeyString = "right"

let gLeftOperandName = Name(gLeftOperandKeyString)
let gRightOperandName = Name(gRightOperandKeyString)


//**********************************************************************



extension Command { // convenience constructors used by operator parsefuncs
    
    static let _NameLeft    = Name("left")
    static let _NameRight   = Name("right")
    
    convenience init(_ name: String, leftOperand: Value) {
        self.init(name, Pair(gLeftOperandName, leftOperand))
    }
    convenience init(_ name: String, leftOperand: Value, rightOperand: Value) {
        self.init(name, Pair(gLeftOperandName, leftOperand),
                                     Pair(gRightOperandName, rightOperand))
    }
    convenience init(_ name: String, rightOperand: Value) {
        self.init(name, Pair(gRightOperandName, rightOperand))
    }
}


// standard prefix/infix/postfix operator parsing functions

// caution: postfix ops MUST label their operand `gNameRight` to avoid any possible confusion with prefix ops of the same name, as they cannot be distinguished by number of arguments alone (the above convenience constructors will label all operands automatically and are recommended for constructing commands for all unary and binary operators)

func parseAtomOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName)
}

func parsePrefixOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName, leftOperand: try parser.parseExpression(precedence))
}

func parseInfixOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName, leftOperand: leftExpr, rightOperand: try parser.parseExpression(precedence))
}

func parseRightInfixOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName, leftOperand: leftExpr, rightOperand: try parser.parseExpression(precedence-1))
}

func parsePostfixOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName, rightOperand: leftExpr)
}


// custom parsefuncs


func readTypeOperand(parser: Parser, precedence: Int) throws -> Command { // precedence is that of `as` operator
    let expr = try parser.parseExpression(precedence) // TO DO: avoid hardcoded values
    switch expr { // TO DO: Value classes should implement cast methods, avoiding need for switches
    case is Command: return expr as! Command
    case is Name: return Command(name: expr as! Name)
    default: throw SyntaxError(description: "Expected type command after `as` operator but found \(expr.dynamicType): \(expr)")
    }
}


private let gAsOperatorName = "as"
private let gAsOperatorPrecedence = 80


func parseGeneralComparisonOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    let rightOperand = try parser.parseExpression(precedence)
    let nextToken = parser.lexer.lookaheadBy(1)// TO DO: if next token is .Operator("as"), consume it and then consume its RH operand and use that as the type to which both operands should be cast before comparing; if not found, use default `text` type instead
    var operands = [leftExpr!, rightOperand]
    if nextToken.type == .Operator && nextToken.value == gAsOperatorName { // TO DO: avoid hardcoded values
        parser.lexer.advance() // advance onto .Operator("as") token
        operands.append(Pair(Name(gAsOperatorName), try readTypeOperand(parser, precedence: gAsOperatorPrecedence)))
    }
    return Command(operatorName, Record(operands))
}


func parseCastOperator(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName, leftOperand: leftExpr, rightOperand: try readTypeOperand(parser, precedence: precedence))
}



func parseProcedureDefinition(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    // TO DO: implement
    // 1. read quoted/unquoted name
    // 2. read record, if given, or require some kind of delimiter (colon? comma?)
    // 3. read group expression; need to decide what structures are appropriate for this, e.g. single line of comma-separated exprs with period terminator (which will require lexer/parser mods), multi-line `do...done` block. bear in mind too that we currently don't have a clearly defined way to express return type; e.g. might define operator as `to SIG: EXPR [returning TYPE]`, though need to consider how well that'd work with single-line exprs (which'd want to put a period after TYPE, not before `returning`)
    throw NotImplementedError()
}


func parseAtomDoBlock(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    var result = [Value]()
    let lexer = parser.lexer
    while lexer.currentToken.type != .EndOfCode {
        while lexer.lookaheadBy(1).type == .LineBreak { // eat any linebreaks before testing for `done`
            lexer.advance()
        }
        let nextToken = lexer.lookaheadBy(1)
        if nextToken.type == .Operator && nextToken.value == "done" { break }
        // TO DO: how best to implement this parse loop? currently this fails if there's a linebreak before `done` as unlike parseExpression this does not automatically skip linebreak tokens (not that it should anyway, since that info should be preserved for display purposes)
//        print("READING EXPR AFTER \(lexer.currentToken):\n\t\t\(nextToken) ...")
        let value = parser.stripPeriod(try parser.parseExpression())
        result.append(value)
//        print("READ EXPR:", value)
    }
//    print("NOW ON \(parser.lexer.currentToken)")
    lexer.advance() // move onto `done` token
    if !(lexer.currentToken.type == .Operator && lexer.currentToken.value == "done") {
        throw SyntaxError(description: "Expected end of `do...done` block but found \(lexer.currentToken)")
    }
    return ExpressionGroup(expressions: result) // TO DO: annotate to indicate it uses `do...done` rather than `(...)` syntax?
}


func parsePostfixDoBlock(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    // TO DO: ditto, then attach it to leftExpr (how best to do this? and should it only apply to commands [including names], or would the be uses in attaching it to other things as well?)
    let doBlock = try parseAtomDoBlock(parser, leftExpr: leftExpr, operatorName: operatorName, precedence: precedence)
    print("TO DO: ATTACH POSTFIX DO BLOCK:\n\t", doBlock, "\n\tTO:", leftExpr)
    return leftExpr
}






func throwMisplacedToken(parser: Parser, leftExpr: Value!, operatorName: String, precedence: Int) throws -> Value {
    throw SyntaxError(description: "Found misplaced \(parser.currentToken.type) token: \(parser.currentToken.value)") // TO DO: should probably throw sub-error here, and leave parser to construct full error message and throw as SyntaxError
}



// Standard operators

// TO DO: should operators be grouped by category (math, etc) where practical, allowing them to be selectively loaded/unloaded? (that alternative would be to define each operator on corresponding proc, but don't think that level of granularity will make code any easier to follow); or is it enough just to define operator table within library, and leave library client to indicate whether or not it wants to import library's operator definitions in addition to its procs

let StandardOperators: [OperatorDefinition] = [ // .Symbol operators will be detected using both whole-word AND in-word (by-char) matching; .Keyword operators by whole-word only
    
    // TO DO: is it worth including user-viewable metadata (e.g. short descriptions) here, or is it best to leave all such metadata for commands to provide? (i.e. is there any metadata that a command wouldn't have but an operator should?)
    
    // TO DO: disambiguating `-` (neg op/sub op/in-word hyphen) is going to be problematic
    
    // TO DO: also consider `-n`; this is presumably intended to negate the value of `n` but as written refers to slot named "-n" instead (changing it to right-auto-delimit might fix this, but need to check)
    
    // note: `+` and `-` right-auto-delimit only (the presence/absence of explicit left-delimiter should be sufficient to distinguish inter-word hyphen from negation/subtraction operator); e.g. `-x` will be read as a prefix `-` (negation) operator with `x` operand, but `foo-bar` will be read as unquoted name where `-` is just an ordinary hyphen character in a hyphenated word (sidenote: `0-9` will still read as infix `-` (subtraction) operator since numbers always self-delimit). Aggressive normalization ("autocorrect") of user code as it is typed should avoid confusion, as symbols determined to be operators will have any "missing" whitespace automatically inserted on each side by pretty printer, and will be styled differently to non-operator chars in editor.
    
    // note: operator names are defined as tuples of form: (canonical/alias name, char/word-based literal, auto-delimit option)
    
    // 'calculation' operators
    
    // arithmetic
    (("^", .Symbol, .Full),   500, .Infix,  parseRightInfixOperator, []), // TO DO: what to use as exponent operator? (`^`, `exp`?)
    (("+", .Symbol, .Right),  490, .Prefix, parsePrefixOperator,     []),
    (("-", .Symbol, .Right),  490, .Prefix, parsePrefixOperator,     [("–", .Symbol, .None)]), // note: also accepts n-dash as synonym // TO DO: not 100% decided about this, as n- and m-dashes could arguably be used in unquoted text to mean themselves
    (("×", .Symbol, .Full),   480, .Infix,  parseInfixOperator,      [("*", .Symbol, .Full)]),
    (("÷", .Symbol, .Full),   480, .Infix,  parseInfixOperator,      [("/", .Symbol, .Full)]),
    (("+", .Symbol, .Right),  470, .Infix,  parseInfixOperator,      []),
    (("-", .Symbol, .Right),  470, .Infix,  parseInfixOperator,      [("–", .Symbol, .None)]), // note: also accepts n-dash as synonym // TO DO: subtraction and hyphenation symbol is same, so adjoining whitespace is required to distinguish the two
    (("div", .Phrase, .Full), 480, .Infix,  parseInfixOperator,      []), // TO DO: allow `//` as alias?
    (("mod", .Phrase, .Full), 480, .Infix,  parseInfixOperator,      []), // TO DO: allow whitespace-delimited `%` as alias? (note: `%` is a unit suffix)
    
    // numeric comparisons (non-ASCII symbols have ASCII aliases for alternative input)
    ((">", .Symbol, .Full),   400, .Infix,  parseInfixOperator, []),
    (("<", .Symbol, .Full),   400, .Infix,  parseInfixOperator, []),
    (("=", .Symbol, .Full),   400, .Infix,  parseInfixOperator, [("==", .Symbol, .Full)]),
    (("≠", .Symbol, .Full),   400, .Infix,  parseInfixOperator, [("!=", .Symbol, .Full)]),
    (("≤", .Symbol, .Full),   400, .Infix,  parseInfixOperator, [("<=", .Symbol, .Full), ("!<", .Symbol, .Full)]),
    (("≥", .Symbol, .Full),   400, .Infix,  parseInfixOperator, [(">=", .Symbol, .Full), ("!>", .Symbol, .Full)]),
    
    // concatenation
    (("&", .Symbol, .Full),   450, .Infix,  parseInfixOperator, []),

    
    // non-numeric comparisons (e.g. text); note that parsefunc overrides standard precedence to allow `COMP as TYPE` to specify comparison type, e.g. `A is B as C` is shorthand for `(A as C) is (B as C)` (currently, `as` operator binds lowest, so would apply to comparison's result, but since these ops always return boolean that isn't really useful, whereas applying cast to both operands prior to comparison is, and allows things like case-insensitive text comparisons and list of X comparisons to be done as well) -- note that a failed cast will throw error (not sure if catching this should be done by typespec, and if it is then what's to prevent `A is not B as C` returning true when both A and B fail to cast causing typespec to supply identical default value for each)
   
    // TO DO: `is before/after or equal to` names are problematic for length; would it be better to use `is not after/before` instead? (these have benefit of brevity, but while self-explanatory in themselves they are much harder to relate to their symbolic equivalents)
    
    // note: `is` and `is not` shortcuts are not right-auto-delimited as they may often appear at start of, or within, longer user-defined command names, particularly those that return a boolean result, e.g. `is job done`
    // caution: it is safer to use longest match for canonical name, e.g. `A is equal B` will rewrite to `A is equal to equal B`, making the mistake obvious, whereas shortest would rewrite to `A is equal B`, concealing it (a highlighting editor would still give some clue since "is" and "equal B" would be colored differently, but a less experienced user could miss that whereas an extra word is much harder to overlook; using longest match as canonical name should also be more auto-complete friendly)
    (("is before",             .Phrase, .Full), 400, .Infix, parseGeneralComparisonOperator, [("lt",                    .Phrase, .Full)]),
    (("is before or equal to", .Phrase, .Full), 400, .Infix, parseGeneralComparisonOperator, [("le",                    .Phrase, .Full),
                                                                                              ("is or before",          .Phrase, .Full),
                                                                                              ("is equal or before",    .Phrase, .Full),
                                                                                              ("is equal to or before", .Phrase, .Full),
                                                                                              ("is not after",          .Phrase, .Full)]),
    (("is after",              .Phrase, .Full), 400, .Infix, parseGeneralComparisonOperator, [("gt",                    .Phrase, .Full)]),
    (("is after or equal to",  .Phrase, .Full), 400, .Infix, parseGeneralComparisonOperator, [("ge",                    .Phrase, .Full),
                                                                                              ("is or after",           .Phrase, .Full),
                                                                                              ("is equal or after",     .Phrase, .Full),
                                                                                              ("is equal to or after",  .Phrase, .Full),
                                                                                              ("is not before",         .Phrase, .Full)]),
    (("is not equal to",       .Phrase, .Full), 400, .Infix, parseGeneralComparisonOperator, [("ne",                    .Phrase, .Full),
                                                                                              ("is not",                .Phrase, .Left)]),
    (("is equal to",           .Phrase, .Full), 400, .Infix, parseGeneralComparisonOperator, [("eq",                    .Phrase, .Full),
                                                                                              ("is",                    .Phrase, .Left)]),
    
    // TO DO: `contains`, `is in`, `starts with`, `ends with` operators, and complements (`does not contain`, etc)
    
    // Boolean
    (("not",     .Phrase, .Right), 100, .Prefix,  parsePrefixOperator, []),
    (("and",     .Phrase,  .Full),  98, .Infix,   parseInfixOperator,  []),
    (("xor",     .Phrase,  .Full),  96, .Infix,   parseInfixOperator,  []), // note: `!=` only works as XOR if both operands are already Bools, whereas `xor` will coerce its operands as necessary, and is visually self-explanatory
    (("or",      .Phrase,  .Full),  94, .Infix,   parseInfixOperator,  []),
    
    // 'clause' operators (these *combine* expressions into more complex behaviors)
    
    // cast
    ((gAsOperatorName, .Phrase, .Full), gAsOperatorPrecedence, .Infix, parseCastOperator, []), // note: "as" may also used as optional 'clause' to some 'binary' operators (really multifix operators, e.g. `A is B as C`)
    
    // reference
    (("of",       .Phrase, .Full), 800, .Infix,   parseInfixOperator,  []), // TO DO: what precedence?
    (("thru",     .Phrase, .Full),  50, .Infix,   parseInfixOperator,  [("through", .Phrase, .Full), ("…", .Symbol, .Full)]), // range constructor; note: symbolic equivalent (ellipsis) is currently included for experimentation only (also, the formatter is not smart enough to preserve the symbolic form where used; thus it'd need to be defined as another operator with its own [aliased] proc for its syntax to be preserved when pretty printed) // TO DO: optional `by` clause for specifying step size?
    (("where",    .Phrase, .Full),  50, .Infix,   parseInfixOperator,  [("whose", .Phrase, .Full)]),  // filter clause; TO DO: RH operand is basically a DSL for constructing queries, albeit using existing operator definitions only (only the procs would be remapped by a sub-context created by 'whose' proc for purpose of evaluating that operand [note that 'where' proc would define its RH operand's type as `test expression`/`Boolean expression` [depending on how we eventually implement]])
    // TO DO: what about `named` and `id` prefix ops for constructing by-name and by-id selectors?
    
    // eval clauses
    (("catching", .Phrase, .Full),  50, .Infix,   parseInfixOperator,  []), // evaluate LH operand; on error, evaluate RH operand
    (("else",     .Phrase, .Full),  50, .Infix,   parseInfixOperator,  []), // evaluate LH operand; if it returns 'did nothing', evaluate RH operand
    
    // 'grouping' operators (these group expressions, e.g. as block or procedure)
    
    // define multi-line expression groups
    // note that `do` and `done` are left-auto-delimited and right-auto-delimited respectively; being intended for writing multi-line expression groups, they should normally be followed/preceded by LineBreak tokens that will explicitly delimit them on the other side. This reduces chance of conflicts when used within a longer
    // expression blocks
    (("do",       .Phrase, .Left),  50, .Atom,    parseAtomDoBlock,    []),
    (("do",       .Phrase, .Left),  50, .Postfix, parsePostfixDoBlock, []),
    (("done",     .Phrase, .Right),  0, .Atom,    throwMisplacedToken, []), // note: `do` block parsefuncs look for `.Operator(done)` to indicate end of block; if `done` keyword is encountered anywhere else, `throwMisplacedToken` automatically reports a syntax error // TO DO: still tempted to call this `end`
    
    // define native procedure
    // `to` operator provides cleaner syntax for defining new procs; as a commonly-used word it is auto-right-delimited only to reduce chance of conflicts when used within a longer unquoted name (e.g. `go back to start`), so must appear at start of an expression to act as operator.
    (("to",     .Phrase, .Right),   0, .Prefix,   parseProcedureDefinition, []),
]



struct OperatorPart<ElementType: Hashable>: CustomStringConvertible { // ElementType is String or Character // TO DO: struct or class?
    // An operator name is composed of one or more 'words'. In a keyword-based operator, e.g. "is not equal to", each word is a String, created by splitting the full name on interstitial whitespace. (In a symbol-based operator, e.g. "!=", the full name should always be a single String-based word.) The PunctuationLexer outputs single String-based words, e.g. ["is", "not", "same", "as"], so to match an entire operator, each defined operator name is first broken down into nested dictionaries, each of whose keys are a single 'word' to match, and whose values are the next matchable word[s] (if any) and/or an operator definition (if a full match has been made).
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


// lookup table for Keyword (e.g. "is not equal to") or Symbol ("!=") operators
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



