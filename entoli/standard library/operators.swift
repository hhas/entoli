//
//  operators.swift
//  entoli
//
//
// TO DO: figure how to package this as a standard module (the same procedures + [optional] operators architecture should work across all libraries; users should be able to import a module's procs, and optionally import the module's operator sugar on top; in addition, sugar may need its own versioning scheme so that a script can continue to use an older, known, sugar even when the module is updated - that allows modules to add new operators without accidentally injecting them into existing scripts that only expect the old operators)
//

// TO DO: if using `!` and `?` as expression separators that also work as behavioural modifiers, it's probably simplest to implement them as postfix operators and convert them to commands that wrap the preceding expression, annotating them with formatter hints to suppress trailing `,`/`.` (Problem: `!` and `?` don't fit well into the 'comma = expr separator, period = block separator' idiom.)


// TO DO: operator parsefuncs should always annotate command to indicate it was created from operator; that'll allow pretty printer to preserve the user's preferred format (actually, it might be better just to use the operator format when it's available; as long as copy&paste operates on ASTs rather than raw text it'll always transfer code in operator-less format between scripts)


// TO DO: update `parseProcedureDefinition` to take a COMMAND:EXPR pair; also need to decide how best to pass optional return type, e.g. `to foo {x:text, y: optional number} returning text: ...`, which also raises question of whether to accept a true pair or whether to read the colon directly, as if `returning` is part of the `to` operator (which would be preferable as it probably doesn't have any value as a standalone operator) then the colon needs to be part of the `to` operator too (it can't be an actual Pair token, because that sort of interleaving isn't supported by Parser). Still need to think through what users are most likely to write and make sure parsefunc can handle those (e.g. auto-correct should also accept `to NAME{}EXPR`, `to NAME,EXPR`, `to NAME returning TYPE,EXPR` and normalize them) as it's such an important operator it can't afford to frustrate.


// TO DO: if `returning` was an infix operator, it could be used to describe return type in situations where `as` is unsuitable, e.g. `BLOCK returning expression` would eval the block, requiring its result to be an expr, whereas `BLOCK as expression` returns the [thunked] block without evaluating it; thus `(foo {x: number} returning text: EXPR)` or `({x: number} returning text: EXPR)` would describe procedures/lambdas using standard syntax. Main issue is context-sensitivity, as the meaning of `returning` would change - it might be a little like a Thunk in that it just wraps the LH operand in a custom pair, in which case it's up to consumer to tell that pair what it wants done (eval expr with given return type [BLOCK as expression]; use pair as-is [PARAMTYPE returning RESULTTYPE])


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
    
    // TO DO: need a human- and machine-readable string-based notation for describing operator syntax, possibly incorporated into `ParseFunc` enums; documentation generator, auto-complete, etc can then use this to describe a command's operator syntax when available
    
    // note: operator names are defined as tuples of form: (canonical/alias name, char/word-based literal, auto-delimit option)
    
    // 'calculation' operators
    
    // arithmetic
    // exponent
    (("^", .symbol, .full),   500, .infix(parseRightInfixOperator),  []), // TO DO: what to use as exponent operator? (`^`, `exp`?)
    // prefix + and - (negation) operators (technically `+` is a no-op [assuming it gets a numeric operand] but is included for symmetry)
    (("+", .symbol, .right),  490, .prefix(parsePrefixOperator),     []),
    (("-", .symbol, .right),  490, .prefix(parsePrefixOperator),     [("–" /* allow n-dash to be used as synonym */, .symbol, .none)]), // caution: `-` right-auto-delimits only; this ensures that `foo-bar` will parse as a single hyphenated word, but does mean users need to be more careful in their use of white space to disambiguate // TO DO: not 100% decided about allowing n-dash as synonyms, as n- and m-dashes could arguably be used in unquoted text to mean themselves (OTOH, what unquoted text would need them?)
    // multiplication and division (traditional computing symbols are accepted as synonyms for ease of typing)
    (("×", .symbol, .full),   480, .infix(parseInfixOperator),       [("*", .symbol, .full)]),
    (("÷", .symbol, .full),   480, .infix(parseInfixOperator),       [("/", .symbol, .full)]),
    // infix + (addition) and - (subtraction) operators
    (("+", .symbol, .right),  470, .infix(parseInfixOperator),       []),
    (("-", .symbol, .right),  470, .infix(parseInfixOperator),       [("–", .symbol, .none)]), // note: also accepts n-dash as synonym // TO DO: subtraction and hyphenation symbol is same, so adjoining whitespace is required to distinguish the two
    // integer division and modulus
    (("div", .phrase, .full), 480, .infix(parseInfixOperator),       []), // TO DO: allow `//` as alias?
    (("mod", .phrase, .full), 480, .infix(parseInfixOperator),       []), // TO DO: allow whitespace-delimited `%` as alias? (note: `%` is a unit suffix)
    
    // numeric comparisons (non-ASCII symbols have ASCII aliases for alternative input)
    ((">", .symbol, .full),   400, .infix(parseInfixOperator), []),
    (("<", .symbol, .full),   400, .infix(parseInfixOperator), []),
    (("=", .symbol, .full),   400, .infix(parseInfixOperator), [("==", .symbol, .full)]),
    (("≠", .symbol, .full),   400, .infix(parseInfixOperator), [("!=", .symbol, .full)]),
    (("≤", .symbol, .full),   400, .infix(parseInfixOperator), [("<=", .symbol, .full), ("!<", .symbol, .full)]),
    (("≥", .symbol, .full),   400, .infix(parseInfixOperator), [(">=", .symbol, .full), ("!>", .symbol, .full)]),
    
    // concatenation
    (("&", .symbol, .full),   450, .infix(parseInfixOperator), []), // TO DO: as with `is equal to`, should this accept optional `as` clause for specifying which type to force both operands to before joining (e.g. `A & B as list of text`); and, if so, how should constraints be applied? // TO DO: what about `|` for merging records rather than concatenating)? (i.e. since record fields may be unnamed, not sure if `&` c.f. AS is appropriate; or is it sufficient just to have `merge` command?); also, when applying `&` to records, should it throw if both records contain same field name? (may also want optional `as` clause, which might solve a lot of these problems)
    
    
    // non-numeric comparisons (e.g. text); note that parsefunc overrides standard precedence to allow `COMP as TYPE` to specify comparison type, e.g. `A is B as C` is shorthand for `(A as C) is (B as C)` (currently, `as` operator binds lowest, so would apply to comparison's result, but since these ops always return boolean that isn't really useful, whereas applying cast to both operands prior to comparison is, and allows things like case-insensitive text comparisons and list of X comparisons to be done as well) -- note that a failed cast will throw error (not sure if catching this should be done by typespec, and if it is then what's to prevent `A is not B as C` returning true when both A and B fail to cast causing typespec to supply identical default value for each)
    
    // TO DO: the default `is before/after or equal to` names are problematic for length; would it be better to use `is not after/before` instead? (these have benefit of brevity, but while self-explanatory in themselves they are much harder to relate to their symbolic equivalents)
    
    // note: `is` and `is not` shortcuts are not right-auto-delimited as they may often appear at start of, or within, longer user-defined command names, particularly those that return a boolean result, e.g. `is job done`
    // caution: it is safer to use longest match for canonical name, e.g. `A is equal B` will rewrite to `A is equal to equal B`, making the mistake obvious, whereas shortest would rewrite to `A is equal B`, concealing it (a highlighting editor would still give some clue since "is" and "equal B" would be colored differently, but a less experienced user could miss that whereas an extra word is much harder to overlook; using longest match as canonical name should also be more auto-complete friendly)
    // important: operators that take an optional `as` clause MUST have higher precendence than the `as` token in order, e.g. `"foo" is "FOO" as case-insensitive text`
    // TO DO: if splitting operators into task-specific sets, they will need some way to indicate if `as` or other 'standard' tokens are also required
    (("is before",             .phrase, .full), 400, .infix(parseGeneralComparisonOperator), [
        ("lt",                    .phrase, .full)]),
    (("is before or equal to", .phrase, .full), 400, .infix(parseGeneralComparisonOperator), [
        ("le",                    .phrase, .full),
        ("is or before",          .phrase, .full),
        ("is equal or before",    .phrase, .full),
        ("is equal to or before", .phrase, .full),
        ("is not after",          .phrase, .full)]),
    (("is after",              .phrase, .full), 400, .infix(parseGeneralComparisonOperator), [
        ("gt",                    .phrase, .full)]),
    (("is after or equal to",  .phrase, .full), 400, .infix(parseGeneralComparisonOperator), [
        ("ge",                    .phrase, .full),
        ("is or after",           .phrase, .full),
        ("is equal or after",     .phrase, .full),
        ("is equal to or after",  .phrase, .full),
        ("is not before",         .phrase, .full)]),
    (("is not equal to",       .phrase, .full), 400, .infix(parseGeneralComparisonOperator), [
        ("ne",                    .phrase, .full),
        ("is not",                .phrase, .left)]),
    (("is equal to",           .phrase, .full), 400, .infix(parseGeneralComparisonOperator), [
        ("eq",                    .phrase, .full),
        ("is",                    .phrase, .left)]),
    
    // TO DO: `contains`, `is in`, `starts with`, `ends with` operators, and complements (`does not contain`, etc)
    
    // Boolean // TO DO: might be best to uppercase these operator names to avoid misuse by novices who use them as English conjunctions instead of logic operators
    (("not",     .phrase, .right), 100, .prefix(parsePrefixOperator), []),
    (("and",     .phrase,  .full),  98, .infix(parseInfixOperator),   []),
    (("xor",     .phrase,  .full),  96, .infix(parseInfixOperator),   []), // note: `!=` only works as XOR if both operands are already Bools, whereas `xor` will coerce its operands as necessary, and is visually self-explanatory
    (("or",      .phrase,  .full),  94, .infix(parseInfixOperator),   []),
    
    // 'clause' operators (these *combine* expressions into more complex behaviors)
    
    // cast
    ((gAsOperatorKeyString, .phrase, .full), gAsOperatorPrecedence, .infix(parseCastOperator), []), // note: "as" may also used as optional 'clause' to some 'binary' operators (really multifix operators, e.g. `A is B as C`)
    
    // reference
    (("of",       .phrase, .full), 800, .infix(parseInfixOperator),  []), // TO DO: what precedence?
    (("thru",     .phrase, .full),  50, .infix(parseInfixOperator),  [("through", .phrase, .full), ("…", .symbol, .full)]), // range constructor; note: symbolic equivalent (ellipsis) is currently included for experimentation only (also, the formatter is not smart enough to preserve the symbolic form where used; thus it'd need to be defined as another operator with its own [aliased] proc for its syntax to be preserved when pretty printed) // TO DO: optional `by` clause for specifying step size? // Q. would `x is in 1 thru 5` be acceptable alternative to `1 ≤ x ≤ 5`? if this the sort of thing we should encourage, or does that way does AS synonym/homonym hell lie? (also, if step is given, then it becomes a set membership test, since range is now discrete values rather than continuum, so gets very sticky very fast)
    (("where",    .phrase, .full),  50, .infix(parseInfixOperator),  [("whose", .phrase, .full)]),  // filter clause; TO DO: RH operand is basically a DSL for constructing queries, albeit using existing operator definitions only (only the procs would be remapped by a sub-context created by 'whose' proc for purpose of evaluating that operand [note that 'where' proc would define its RH operand's type as `test expression`/`Boolean expression` [depending on how we eventually implement]])
    // TO DO: what about `named` and `id` prefix ops for constructing by-name and by-id selectors?
    
    // adverbs
    (("catching", .phrase, .full),  50, .infix(parseInfixOperator),  []), // evaluate LH operand; on error, evaluate RH operand
    (("else",     .phrase, .full),  50, .infix(parseInfixOperator),  []), // evaluate LH operand; if it returns 'did nothing', evaluate RH operand
    
    // 'grouping' operators (these group expressions, e.g. as block or procedure)
    
    // define multi-line expression groups
    // note that `do` and `done` are left-auto-delimited and right-auto-delimited respectively; being intended for writing multi-line expression groups, they should normally be followed/preceded by LineBreak tokens that will explicitly delimit them on the other side. This reduces chance of conflicts when used within a longer
    // expression blocks
    (("do",       .phrase, .left),  50, .atom(parseAtomDoBlock),       []),
    (("do",       .phrase, .left),  50, .postfix(parsePostfixDoBlock), []), // TO DO: might argue for using colon pairs; is there anywhere other than `to PROCNAME do ... done` where postfixed do blocks are used? (the rationale is that the do block appends to argument record, similar to how semicolon pair prefixes to argument record); the flipside is that it might be better as an autocorrect in this particular use case; also bear in mind that colon becomes required when using period-terminated blocks (e.g. `To draw square: forward 100, turn left 90,..., forward 100.`) Also avoids any confusion when writing lambdas, e.g. `{arg}:cmd,cmd.`, `{arg}:do...done.`
    (("done",     .phrase, .right),  0, .atom(parseMisplacedToken),    []), // note: `do` block parsefuncs look for `.Operator(done)` to indicate end of block; if `done` keyword is encountered anywhere else, `parseMisplacedToken` automatically reports a syntax error // TO DO: still tempted to call this `end` (though what problems could that cause when `end` is used as a method [attribute] name, e.g. `end of some list`, `end of documents`; might work if blocks require it to appear alone on line)
    
    // define native procedure
    // `to` operator provides cleaner syntax for defining new procs; as a commonly-used word it is auto-right-delimited only to reduce chance of conflicts when used within a longer unquoted name (e.g. `go back to start`), so must appear at start of an expression to act as operator.
    (("to",     .phrase, .right),   0, .prefix(parseProcedureDefinition), [("when", .phrase, .right)]), // lowest precedence ensures operator will use rest of expression as its operand
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


func parseGeneralComparisonOperator(_ parser: Parser, leftExpr: Value, operatorName: String, precedence: Int) throws -> Value {
    let rightOperand = try parser.parseExpression(precedence) // note that this should _not_ consume `as` clause itself, as comparison operators [must] have higher precedence than `as` token
    let nextToken = parser.lexer.lookahead(by: 1) // if next token is .Operator("as"), consume it and then consume its RH operand and use that as the type to which both operands should be cast before comparing; if omitted, operator will use default `text` type instead
    var operands = [leftExpr, rightOperand]
    if nextToken.type == .operatorName && nextToken.value == gAsOperatorKeyString {
        parser.lexer.advance() // advance onto .Operator("as") token
        operands.append(Pair(Name(gAsOperatorKeyString), try readTypeOperand(parser, precedence: gAsOperatorPrecedence)))
    }
    return Command(operatorName, Record(operands))
}


func parseCastOperator(_ parser: Parser, leftExpr: Value, operatorName: String, precedence: Int) throws -> Value {
    return Command(operatorName, leftOperand: leftExpr, rightOperand: try readTypeOperand(parser, precedence: precedence))
}


func parseProcedureDefinition(_ parser: Parser, operatorName: String, precedence: Int) throws -> Value {
    // TO DO: this isn't going to work right if operand is a Pair, e.g. `to foo: bar.`; basically need to think about how forgiving syntax (autocorrect) should be, and how much can be done at this point to split it into individual values (name, in/out types, body; also, where should annotations attach?).
    // 1. read quoted/unquoted name, including parameter record, if given
    let procName: Name, parameterType: RecordSignature, returnType: Value
    guard let op1 = try parser.parseAtom() else { throw SyntaxError(description: "Expected procedure's name after `to` operator, ") }
    // TO DO: sort this out; one option is to use `op1.toCommand` which will work for name, command, or parensed name/command
    if let command = op1 as? Command {
        procName = command.name
        parameterType = try command.argument.toRecordSignature() // note: param types will be ProxyConstraints until they can be evaluated
    } else if let name = op1 as? Name {
        procName = name
        parameterType = gEmptyRecordSignature
    } else {
        throw SyntaxError(description: "Expected procedure's name after `to` operator, but found \(op1.typename) value instead: \(op1)")
    }
    // 2. read `as` clause, if given, specifying return type
    let nextToken = parser.lexer.lookahead(by: 1)
    if nextToken.type == .operatorName && nextToken.value == gAsOperatorKeyString {
        parser.lexer.advance() // advance onto .Operator("as") token
        returnType = try readTypeOperand(parser, precedence: gAsOperatorPrecedence) // note: this is a command, not a coercion, as it's still to be evaled // TO DO: this doesn't handle trailing comma (really need to try to get rid of those now)
    } else {
        returnType = gAnythingConstraint
    }
    if [.pairSeparator, .clauseSeparator].contains(nextToken.type) { parser.lexer.advance() } // skip colon after signature (technically speaking, `to SIG:EXPR` is a prefix operator that takes a pair as argument, but we want to forgive imperfect user input so `to SIG,EXPR` and `to SIG EXPR` are parsed as well)
    // 4. read group expression; need to decide what structures are appropriate for this, e.g. single line of comma-separated exprs with period terminator (which will require lexer/parser mods), multi-line `do...done` block. bear in mind too that we currently don't have a clearly defined way to express return type; e.g. might define operator as `to SIG: EXPR [returning TYPE]`, though need to consider how well that'd work with single-line exprs (which'd want to put a period after TYPE, not before `returning`)
    let procBody = try parser.parseExpression()
    
    return makeDefineProcedureCommand(procName, parameterType: parameterType, returnType: returnType, body: procBody)
}



func parseAtomDoBlock(_ parser: Parser, operatorName: String, precedence: Int) throws -> Value { // TO DO: this needs to use same reader as Parser.parseAtom's `(...)` expression in order to parse names and named pairs correctly
    
    // note: `(name:value)` is a bit problematic; the only thing we can do is ensure consistent behavior, always treating as `store` unless in a context that treats it as a pair (list/record/`(...) as pair` coercion; Q. what when it appears as a parameter? e.g. `if {x, name:value}` - presumably that stores too)
    
    // (half-wish we could do bullet lists!)
    
    //let result = try parser.parseExpressionSequence({print("checking \($0)"); return $0.type == .operatorName && $0.value == "done"}, shouldNamedPairsStore: true) // TO DO: this doesn't work right yet
    
    // TO DO: need to give more thought to this
    
    parser.lexer.advance()
    var result = [Value]()
    let lexer = parser.lexer
    while lexer.currentToken.type != .endOfCode {
        while lexer.lookahead(by: 1).type == .lineBreak { // eat any linebreaks before testing for `done`
            lexer.advance()
        }
        let nextToken = lexer.lookahead(by: 1)
        if nextToken.type == .operatorName && nextToken.value == "done" { break }
        // TO DO: how best to implement this parse loop? currently this fails if there's a linebreak before `done` as unlike parseExpression this does not automatically skip linebreak tokens (not that it should anyway, since that info should be preserved for display purposes)
        //        print("READING EXPR AFTER \(lexer.currentToken):\n\t\t\(nextToken) ...")
        let value = try parser.parseExpression()
        result.append(value)
        //        print("READ EXPR:", value)
    }
    //    print("NOW ON \(lexer.currentToken)")
    lexer.advance() // move onto `done` token
    if !(lexer.currentToken.type == .operatorName && lexer.currentToken.value == "done") {
        throw SyntaxError(description: "Expected end of `do...done` block but found \(lexer.currentToken)")
    }
    return ExpressionBlock(expressions: result, format: .block)}


func parsePostfixDoBlock(_ parser: Parser, leftExpr: Value, operatorName: String, precedence: Int) throws -> Value {
    let exprs = try parseAtomDoBlock(parser, operatorName: operatorName, precedence: precedence)
    if let record = leftExpr as? Record {
        return record.appended(exprs)
    } else {
        return Record([leftExpr, exprs])
    }
}



