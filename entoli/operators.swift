//
//  operators.swift
//  entoli
//
//
//

// TO DO: if using `!` and `?` as expression separators that also work as behavioural modifiers, it's probably simplest to implement them as postfix operators and convert them to commands that wrap the preceding expression, annotating them with formatter hints to suppress trailing `,`/`.`


// TO DO: operator parsefuncs should always annotate command to indicate it was created from operator


let StandardOperatorsTable = Operators().add(StandardOperators) // TO DO: where to put this? (both parser and formatter require access to formatting tables, which in turn need to be composed according to what modules are imported)


//**********************************************************************
//


func makeStoreValueCommand(_ value: Value, named: Name) -> Command {
    return Command("store", value, named) // Pair(Name("value"), self.value), Pair(Name("named"), name)? also, make read-only
}

func makeDefineProcedureCommand(_ procName: Name, parameterType: ParameterType, returnType: Value, body: Value) -> Command {
    return Command("to", procName, parameterType, returnType, body)
}


//**********************************************************************
// Standard operators

// TO DO: also need to include formatfuncs

// TO DO: should operators be grouped by category (math, etc) where practical, allowing them to be selectively loaded/unloaded? (that alternative would be to define each operator on corresponding proc, but don't think that level of granularity will make code any easier to follow); or is it enough just to define operator table within library, and leave library client to indicate whether or not it wants to import library's operator definitions in addition to its procs

let StandardOperators: [OperatorDefinition] = [ // .Symbol operators will be detected using both whole-word AND in-word (by-char) matching; .Keyword operators by whole-word only
    
    // TO DO: is it worth including user-viewable metadata (e.g. short descriptions) here, or is it best to leave all such metadata for commands to provide? (i.e. is there any metadata that a command wouldn't have but an operator should?)
    
    // TO DO: disambiguating `-` (neg op/sub op/in-word hyphen) is going to be problematic
    
    // TO DO: also consider `-n`; this is presumably intended to negate the value of `n` but as written refers to slot named "-n" instead (changing it to right-auto-delimit might fix this, but need to check)
    
    // note: `+` and `-` right-auto-delimit only (the presence/absence of explicit left-delimiter should be sufficient to distinguish inter-word hyphen from negation/subtraction operator); e.g. `-x` will be read as a prefix `-` (negation) operator with `x` operand, but `foo-bar` will be read as unquoted name where `-` is just an ordinary hyphen character in a hyphenated word (sidenote: `0-9` will still read as infix `-` (subtraction) operator since numbers always self-delimit). Aggressive normalization ("autocorrect") of user code as it is typed should avoid confusion, as symbols determined to be operators will have any "missing" whitespace automatically inserted on each side by pretty printer, and will be styled differently to non-operator chars in editor.
    
    // note: operator names are defined as tuples of form: (canonical/alias name, char/word-based literal, auto-delimit option)
    
    // 'calculation' operators
    
    // arithmetic
    (("^", .symbol, .full),   500, .infix,  parseRightInfixOperator, []), // TO DO: what to use as exponent operator? (`^`, `exp`?)
    (("+", .symbol, .Right),  490, .prefix, parsePrefixOperator,     []),
    (("-", .symbol, .Right),  490, .prefix, parsePrefixOperator,     [("–", .symbol, .none)]), // note: also accepts n-dash as synonym // TO DO: not 100% decided about this, as n- and m-dashes could arguably be used in unquoted text to mean themselves
    (("×", .symbol, .full),   480, .infix,  parseInfixOperator,      [("*", .symbol, .full)]),
    (("÷", .symbol, .full),   480, .infix,  parseInfixOperator,      [("/", .symbol, .full)]),
    (("+", .symbol, .Right),  470, .infix,  parseInfixOperator,      []),
    (("-", .symbol, .Right),  470, .infix,  parseInfixOperator,      [("–", .symbol, .none)]), // note: also accepts n-dash as synonym // TO DO: subtraction and hyphenation symbol is same, so adjoining whitespace is required to distinguish the two
    (("div", .phrase, .full), 480, .infix,  parseInfixOperator,      []), // TO DO: allow `//` as alias?
    (("mod", .phrase, .full), 480, .infix,  parseInfixOperator,      []), // TO DO: allow whitespace-delimited `%` as alias? (note: `%` is a unit suffix)
    
    // numeric comparisons (non-ASCII symbols have ASCII aliases for alternative input)
    ((">", .symbol, .full),   400, .infix,  parseInfixOperator, []),
    (("<", .symbol, .full),   400, .infix,  parseInfixOperator, []),
    (("=", .symbol, .full),   400, .infix,  parseInfixOperator, [("==", .symbol, .full)]),
    (("≠", .symbol, .full),   400, .infix,  parseInfixOperator, [("!=", .symbol, .full)]),
    (("≤", .symbol, .full),   400, .infix,  parseInfixOperator, [("<=", .symbol, .full), ("!<", .symbol, .full)]),
    (("≥", .symbol, .full),   400, .infix,  parseInfixOperator, [(">=", .symbol, .full), ("!>", .symbol, .full)]),
    
    // concatenation
    (("&", .symbol, .full),   450, .infix,  parseInfixOperator, []), // TO DO: as with `is equal to`, should this accept optional `as` clause for specifying which type to force both operands to before joining (e.g. `A & B as list of text`); and, if so, how should constraints be applied? // TO DO: what about `|` for merging records rather than concatenating)? (i.e. since record fields may be unnamed, not sure if `&` c.f. AS is appropriate; or is it sufficient just to have `merge` command?); also, when applying `&` to records, should it throw if both records contain same field name? (may also want optional `as` clause, which might solve a lot of these problems)
    
    
    // non-numeric comparisons (e.g. text); note that parsefunc overrides standard precedence to allow `COMP as TYPE` to specify comparison type, e.g. `A is B as C` is shorthand for `(A as C) is (B as C)` (currently, `as` operator binds lowest, so would apply to comparison's result, but since these ops always return boolean that isn't really useful, whereas applying cast to both operands prior to comparison is, and allows things like case-insensitive text comparisons and list of X comparisons to be done as well) -- note that a failed cast will throw error (not sure if catching this should be done by typespec, and if it is then what's to prevent `A is not B as C` returning true when both A and B fail to cast causing typespec to supply identical default value for each)
    
    // TO DO: the default `is before/after or equal to` names are problematic for length; would it be better to use `is not after/before` instead? (these have benefit of brevity, but while self-explanatory in themselves they are much harder to relate to their symbolic equivalents)
    
    // note: `is` and `is not` shortcuts are not right-auto-delimited as they may often appear at start of, or within, longer user-defined command names, particularly those that return a boolean result, e.g. `is job done`
    // caution: it is safer to use longest match for canonical name, e.g. `A is equal B` will rewrite to `A is equal to equal B`, making the mistake obvious, whereas shortest would rewrite to `A is equal B`, concealing it (a highlighting editor would still give some clue since "is" and "equal B" would be colored differently, but a less experienced user could miss that whereas an extra word is much harder to overlook; using longest match as canonical name should also be more auto-complete friendly)
    // important: operators that take an optional `as` clause MUST have higher precendence than the `as` token in order, e.g. `"foo" is "FOO" as case-insensitive text`
    // TO DO: if splitting operators into task-specific sets, they will need some way to indicate if `as` or other 'standard' tokens are also required
    (("is before",             .phrase, .full), 400, .infix, parseGeneralComparisonOperator, [("lt",                    .phrase, .full)]),
    (("is before or equal to", .phrase, .full), 400, .infix, parseGeneralComparisonOperator, [("le",                    .phrase, .full),
        ("is or before",          .phrase, .full),
        ("is equal or before",    .phrase, .full),
        ("is equal to or before", .phrase, .full),
        ("is not after",          .phrase, .full)]),
    (("is after",              .phrase, .full), 400, .infix, parseGeneralComparisonOperator, [("gt",                    .phrase, .full)]),
    (("is after or equal to",  .phrase, .full), 400, .infix, parseGeneralComparisonOperator, [("ge",                    .phrase, .full),
        ("is or after",           .phrase, .full),
        ("is equal or after",     .phrase, .full),
        ("is equal to or after",  .phrase, .full),
        ("is not before",         .phrase, .full)]),
    (("is not equal to",       .phrase, .full), 400, .infix, parseGeneralComparisonOperator, [("ne",                    .phrase, .full),
        ("is not",                .phrase, .Left)]),
    (("is equal to",           .phrase, .full), 400, .infix, parseGeneralComparisonOperator, [("eq",                    .phrase, .full),
        ("is",                    .phrase, .Left)]),
    
    // TO DO: `contains`, `is in`, `starts with`, `ends with` operators, and complements (`does not contain`, etc)
    
    // Boolean
    (("not",     .phrase, .Right), 100, .prefix,  parsePrefixOperator, []),
    (("and",     .phrase,  .full),  98, .infix,   parseInfixOperator,  []),
    (("xor",     .phrase,  .full),  96, .infix,   parseInfixOperator,  []), // note: `!=` only works as XOR if both operands are already Bools, whereas `xor` will coerce its operands as necessary, and is visually self-explanatory
    (("or",      .phrase,  .full),  94, .infix,   parseInfixOperator,  []),
    
    // 'clause' operators (these *combine* expressions into more complex behaviors)
    
    // cast
    ((gAsOperatorKeyString, .phrase, .full), gAsOperatorPrecedence, .infix, parseCastOperator, []), // note: "as" may also used as optional 'clause' to some 'binary' operators (really multifix operators, e.g. `A is B as C`)
    
    // reference
    (("of",       .phrase, .full), 800, .infix,   parseInfixOperator,  []), // TO DO: what precedence?
    (("thru",     .phrase, .full),  50, .infix,   parseInfixOperator,  [("through", .phrase, .full), ("…", .symbol, .full)]), // range constructor; note: symbolic equivalent (ellipsis) is currently included for experimentation only (also, the formatter is not smart enough to preserve the symbolic form where used; thus it'd need to be defined as another operator with its own [aliased] proc for its syntax to be preserved when pretty printed) // TO DO: optional `by` clause for specifying step size?
    (("where",    .phrase, .full),  50, .infix,   parseInfixOperator,  [("whose", .phrase, .full)]),  // filter clause; TO DO: RH operand is basically a DSL for constructing queries, albeit using existing operator definitions only (only the procs would be remapped by a sub-context created by 'whose' proc for purpose of evaluating that operand [note that 'where' proc would define its RH operand's type as `test expression`/`Boolean expression` [depending on how we eventually implement]])
    // TO DO: what about `named` and `id` prefix ops for constructing by-name and by-id selectors?
    
    // eval clauses
    (("catching", .phrase, .full),  50, .infix,   parseInfixOperator,  []), // evaluate LH operand; on error, evaluate RH operand
    (("else",     .phrase, .full),  50, .infix,   parseInfixOperator,  []), // evaluate LH operand; if it returns 'did nothing', evaluate RH operand
    
    // 'grouping' operators (these group expressions, e.g. as block or procedure)
    
    // define multi-line expression groups
    // note that `do` and `done` are left-auto-delimited and right-auto-delimited respectively; being intended for writing multi-line expression groups, they should normally be followed/preceded by LineBreak tokens that will explicitly delimit them on the other side. This reduces chance of conflicts when used within a longer
    // expression blocks
    (("do",       .phrase, .Left),  50, .atom,    parseAtomDoBlock,    []),
    (("do",       .phrase, .Left),  50, .postfix, parsePostfixDoBlock, []),
    (("done",     .phrase, .Right),  0, .atom,    parseMisplacedToken, []), // note: `do` block parsefuncs look for `.Operator(done)` to indicate end of block; if `done` keyword is encountered anywhere else, `parseMisplacedToken` automatically reports a syntax error // TO DO: still tempted to call this `end`
    
    // define native procedure
    // `to` operator provides cleaner syntax for defining new procs; as a commonly-used word it is auto-right-delimited only to reduce chance of conflicts when used within a longer unquoted name (e.g. `go back to start`), so must appear at start of an expression to act as operator.
    (("to",     .phrase, .Right),   0, .prefix,   parseProcedureDefinition, []), // lowest precedence ensures operator will use rest of expression as its operand
]



//**********************************************************************
// custom parsefuncs // TO DO: these and their operator definitions should move to own file


func readTypeOperand(_ parser: Parser, precedence: Int) throws -> Command { // precedence is that of `as` operator
    let expr = try parser.parseExpression(precedence)
    do {
        return try expr.toCommand()
    } catch {
        throw SyntaxError(description: "Expected type command after `as` operator but found \(expr.typename) instead: \(expr)")
    }
}


func parseGeneralComparisonOperator(_ parser: Parser, leftExpr: Value?, operatorName: String, precedence: Int) throws -> Value {
    let rightOperand = try parser.parseExpression(precedence) // note that this should _not_ consume `as` clause itself, as comparison operators [must] have higher precedence than `as` token
    let nextToken = parser.lexer.lookaheadBy(1) // if next token is .Operator("as"), consume it and then consume its RH operand and use that as the type to which both operands should be cast before comparing; if omitted, operator will use default `text` type instead
    var operands = [leftExpr!, rightOperand]
    if nextToken.type == .operator && nextToken.value == gAsOperatorKeyString {
        parser.lexer.advance() // advance onto .Operator("as") token
        operands.append(Pair(Name(gAsOperatorKeyString), try readTypeOperand(parser, precedence: gAsOperatorPrecedence)))
    }
    return Command(operatorName, Record(operands))
}


func parseCastOperator(_ parser: Parser, leftExpr: Value?, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName, leftOperand: leftExpr!, rightOperand: try readTypeOperand(parser, precedence: precedence))
}


func parseProcedureDefinition(_ parser: Parser, leftExpr: Value?, operatorName: String, precedence: Int) throws -> Value {
    // TO DO: this isn't going to work right if operand is a Pair, e.g. `to foo: bar.`; basically need to think about how forgiving syntax (autocorrect) should be, and how much can be done at this point to split it into individual values (name, in/out types, body; also, where should annotations attach?).
    // 1. read quoted/unquoted name, including parameter record, if given
    let procName: Name, parameterType: RecordSignature, returnType: Value
    guard let op1 = try parser.parseAtom() else { throw SyntaxError(description: "Expected procedure's name after `to` operator, ") }
    // TO DO: sort this out; one option is to use `op1.toCommand` which will work for name, command, or parensed name/command
    if let command = op1 as? Command {
        procName = command.name
        parameterType = try command.argument.toRecordSignature() // note: param types will be ProxyCoercions until they can be evaluated
    } else if let name = op1 as? Name {
        procName = name
        parameterType = gEmptyRecordSignature
    } else {
        throw SyntaxError(description: "Expected procedure's name after `to` operator, but found \(op1.typename) value instead: \(op1)")
    }
    // 2. read `as` clause, if given, specifying return type
    let nextToken = parser.lexer.lookaheadBy(1)
    if nextToken.type == .operator && nextToken.value == gAsOperatorKeyString {
        parser.lexer.advance() // advance onto .Operator("as") token
        returnType = try readTypeOperand(parser, precedence: gAsOperatorPrecedence) // note: this is a command, not a coercion, as it's still to be evaled // TO DO: this doesn't handle trailing comma (really need to try to get rid of those now)
    } else {
        returnType = gAnythingCoercion
    }
    // 4. read group expression; need to decide what structures are appropriate for this, e.g. single line of comma-separated exprs with period terminator (which will require lexer/parser mods), multi-line `do...done` block. bear in mind too that we currently don't have a clearly defined way to express return type; e.g. might define operator as `to SIG: EXPR [returning TYPE]`, though need to consider how well that'd work with single-line exprs (which'd want to put a period after TYPE, not before `returning`)
    let procBody = try parser.parseExpression()
    
    return makeDefineProcedureCommand(procName, parameterType: parameterType, returnType: returnType, body: procBody)
    
  //  let proc = NativeProcedure(signature: ProcedureSignature(name: procName, parameterType: parameterType, returnType: returnType), body: procBody)
}


func parseAtomDoBlock(_ parser: Parser, leftExpr: Value?, operatorName: String, precedence: Int) throws -> Value { // TO DO: this needs to use same reader as Parser.parseAtom's `(...)` expression in order to parse names and named pairs correctly
    var result = [Value]()
    let lexer = parser.lexer
    while lexer.currentToken.type != .endOfCode {
        while lexer.lookaheadBy(1).type == .lineBreak { // eat any linebreaks before testing for `done`
            lexer.advance()
        }
        let nextToken = lexer.lookaheadBy(1)
        if nextToken.type == .operator && nextToken.value == "done" { break }
        // TO DO: how best to implement this parse loop? currently this fails if there's a linebreak before `done` as unlike parseExpression this does not automatically skip linebreak tokens (not that it should anyway, since that info should be preserved for display purposes)
        //        print("READING EXPR AFTER \(lexer.currentToken):\n\t\t\(nextToken) ...")
        let value = try parser.parseExpression()
        result.append(value)
        //        print("READ EXPR:", value)
    }
    //    print("NOW ON \(lexer.currentToken)")
    lexer.advance() // move onto `done` token
    if !(lexer.currentToken.type == .operator && lexer.currentToken.value == "done") {
        throw SyntaxError(description: "Expected end of `do...done` block but found \(lexer.currentToken)")
    }
    return ExpressionSequence(expressions: result) // TO DO: annotate to indicate it uses `do...done` rather than `(...)` syntax?
}


func parsePostfixDoBlock(_ parser: Parser, leftExpr: Value?, operatorName: String, precedence: Int) throws -> Value {
    // TO DO: ditto, then attach it to leftExpr (how best to do this? and should it only apply to commands [including names], or would the be uses in attaching it to other things as well?)
    let doBlock = try parseAtomDoBlock(parser, leftExpr: leftExpr, operatorName: operatorName, precedence: precedence)
    print("TO DO: ATTACH POSTFIX DO BLOCK:\n\t", doBlock, "\n\tTO:", leftExpr)
    return leftExpr!
}



