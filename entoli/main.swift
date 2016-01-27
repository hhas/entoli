//
//  main.swift
//  entoli


let source: String

//let code: ScriptChars = "3.2e+5.1".characters; print(readNumericWord(code, start: code.startIndex))

//source = "hello is  equal to \tthat  {user name :1, 'smith', to:A and B, 99:nope} \n run script \"yes\" foo: bar: baz"
//source = " 'one' 'two' 'three' " //  `'one' {'two' {'three'}}.`
//source = " (FOO) is not BAR "   // correct:  `'is not equal to' {('FOO'), 'BAR'}.` (note: `is not` is alias for `is not equal to`)

// obsolete: //source = " (FOO) is not same. as BAR " // correct:  `parseAtom() encountered an infix operator: "as"`

//source = " (FOO) is NOT equal. to BAR " // correct:  `'is not equal to' {('FOO'), 'equal'}. 'to BAR'.` (though this will change once `to` operator is implemented, at which point its parsefunc will throw a 'missing argument' error)

//source = " 4 is equal to not test 3" // correct:  `'is' {"4", 'not' {'test' {"3"}}}.`
//source = "  bob is not smith   !=x   1   /   2   +   4  . " //. 2*2. 3 - 3. 4-4. "
//source = "a*b"  //   correct:   `'×' {'a', 'b'}.`
//source = "a and b" //  correct:  `'and' {'a', 'b'}.`
//source = " 3*4 "  //   correct:  `'×' {"3", "4"}.`
//source = " 3not b"  //   wrong:  produces  `"3". 'not' {'b'}.`, but `not` isn't a new word so should be treated as [unknown] unit suffix to `3` (the parser will warn this doesn't work right yet)
//source = " foo {a label:1, 2, c and not d, 4 + 6 = 10} "
//source = "4 + 6 * 10"         // correct:   `'+' {"4", '×' {"6", "10"}}.`
//source = "4 + 6 < 10"         // correct:   `'<' {'+' {"4", "6"}, "10"}.`
//source = "4 + 6 is after 10"  // correct:   `'is after' {'+' {"4", "6"}, "10"}.`
//source = "4 + 6 != 10"          // correct:   `'≠' {'+' {"4", "6"}, "10"}.`
//source = "0-123-7.34" // TO DO: hyphenated numbers currently split into separate negative numbers; should eventually be pattern matched as dates
//source = "forward 50 left 90 forward 50. repeat {4 (forward 50 left 90)}. repeat 4 [forward 50 left 90]"
//source = " repeat 4 do \n  forward 50 left 90  \n done"
//source = "forward 100 left turn, forward 40, left turn 45, forward 10. turn -90 forward 25"
//source = " \"foo\" is not before \"bar\" as case-insensitive text "

//source = " guess: random number {1 thru 10} as lazy. "

//source = " to foo bar {} do \n this, that, the other 42 \n done."

//source = " [a:1], (b:2), {c:3}, d:4, \"e\":5  "

//source = " (2 + 3 * 4 - 1.5) div 1" // -> Text("12")

//source = " [ 2 ,  4. 6 ] " //  EntoliScript([List(Text("2"), Text("4"), Text("6"))])

// EntoliScript([List(Text("2"), Name("foo"), Command(Name("bar"), Record()), (Command(Name("baz"), Record())))])
source = " [2, foo, bar{}, (baz)] " // TO DO: problem: `foo` parses as Name but should eval as Command; OTOH, in a record it needs to eval as command OR name depending on use case, e.g. `foo {bar}` vs `to foo {param}`; also, lookahead is needed to determine if `as` operator is next (which in turn is problematic because `as` is not part of core parser so cannot be relied on to exist); TBH, even `foo{bar}` can't be converted to `foo{bar{}}` by parser as it has no way of knowing what `foo` proc wants to do with `bar`, e.g. it might want to treat it as a name, in which case user would have to parens it to have it eval as command [which hopefully returns a name] instead




let LEXER_TEST = 0

if LEXER_TEST != 0 { // print each token in source
    let lexer = Lexer(code: source)
    var i = 0
    repeat {
        i+=1
        print("==>TOKEN:", lexer.currentToken)
        print("__________________________________________________________________\n")
        lexer.advance()
    } while lexer.currentToken.type != gEndOfCodeToken.type //&& i<4
}





let PARSER_TEST = 0

if PARSER_TEST != 0 { // parse source into AST
    let p = Parser(lexer: Lexer(code: source))
    do {
        let result = try p.parseScript()
        print("Result:", result)
    } catch {
        print("ERROR:", error)
    }
    print("\n================================================\n")
}




//for d in (opt.operatorDefinitionsByWord) {print(d.0)}
//print(ops.infixDefinitions["is"])



do {
    // initialize an environment
    let env = Scope()
    try loadLibrary(env)
    
    
 //   let script = "[3]"
    let script = " [(2 + 3 * 4 - 1.5), 3.14 div 1]"
  //  let script = " store {5, x}. x () " // note: empty expression group is equivalent to passing `{}` or `nothing`
//    let script = " to foo {} 3 + 1. foo " // test native procedure definition (currently doesn't work as ParameterTypeCoercion is TBC)
    
    let value = try Parser(lexer: Lexer(code: script)).parseScript()
    print(script, "=>", try value.evaluate(env, returnType: gAnythingCoercion))
//    print(env)
    print("\n================================================\n")

    
    do {
        let value = Text("2")
        // test Entoli data evaluation
        print("Any:   ", try value.evaluate(env, returnType: gAnyValueCoercion))
        print("Text:  ", try value.evaluate(env, returnType: gTextCoercion))   // -> "2" (Text)
        // test Entoli->Swift data mapping
        print("String:", try value.evaluate(env, returnType: gStringCoercion)) // -> "2" (String)
        print("Int:   ", try value.evaluate(env, returnType: gDoubleCoercion))    // -> 2   (Int)
        print("Double:", try value.evaluate(env, returnType: gDoubleCoercion)) // -> 2.0 (Double)
        print("")
    }
    
    print("\n================================================\n")
    
    // test procedure call, `+ {2, 3}` (or `2 + 3` if operator syntax sugar is used)
    let command = Command("+", Text("2"), Text("3"))
    let result = try env.callProcedure(command, commandScope: env, returnType: gAnyValueCoercion)
    print(command, "=>", result, "\n") // -> `'+' {"2", "3"}` -> Text("5.0")
    
    // `nothing` procedure simply returns `nothing` constant (aka gNullValue)
    // print(try env.callProcedure(Command("nothing"), commandScope: env, returnType: gAnyValueCoercion)) // -> 'nothing'
    
//    try env.store(Name("wibble")) // define a named "constant" (i.e. a name that evals to itself)
//    print(try env.callProcedure(Command("wibble"), commandScope: env, returnType: gAnyValueCoercion)) // -> 'wibble'
    
    
 //   print("EVALED:", try value.evaluate(env, returnType: ThunkCoercion(returnType: gDoubleCoercion)))   // -> Thunk("2", IntCoercion)
 //   print("EVALED:", try value.evaluate(env, returnType: ThunkCoercion(returnType: gDoubleCoercion)).evaluate(env, returnType: gAnyValueCoercion)) // incorrect; currently returns Text
    
} catch { print("\nA TEST FAILED:",error) }




