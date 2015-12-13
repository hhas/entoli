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
//source = " 3not b"  //   wrong:  produces  `"3". 'not' {'b'}.`, but `not` isn't a new word so should be treated as [unknown] unit suffix to `3`
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

source = " (2 + 3 * 4 - 1.5) div 1" // -> Text("12")


let lexer = Lexer(code: source)


let test1 = 0

if test1 != 0 {
    var i = 0
    repeat {
        i+=1
        print("==>TOKEN:", lexer.currentToken)
        print("__________________________________________________________________\n")
        lexer.advance()
    } while lexer.currentToken.type != gEndOfCodeToken.type //&& i<4
}

let p = Parser(lexer: lexer)


let test2 = 0

if test2 != 0 {
    do {
        let result = try p.parseScript()
        print("\n\n================================================\n", result)
    } catch {
        print("\n\n================================================\nERROR:", error)
    }
}



/*
do {
    while true {
        print("-----------------------")
        let res = try p.parseExpression()
        print(res)
//        print(res.dynamicType)
        print("")
    }
} catch {
    print(error)
}
*/

//while let t = lexer.next() { print(t) }



//for d in (opt.operatorDefinitionsByWord) {print(d.0)}

//print(ops.infixDefinitions["is"])



do {
    // initialize an environment
    let env = Scope()
    try loadLibrary(env)
//    let script = " [(2 + 3 * 4 - 1.5), 3.14] "//div 1 "
 //   let script = "[3]"
    
  //  let script = " store {5, x}. x () " // TO DO: () should be treated same as {} or `nothing` here, either by parser or runtime (not sure which); might be best if parser 'auto-corrected' this
    
    let script = " to foo {} 3 + 1. foo "
    
    
    let value = try Parser(lexer: Lexer(code: script)).parseScript()
    print(script, "=>", try value.evaluate(env, returnType: gAnythingCoercion))
//    print(env)
    
    /*
    do {
        let value = Text("2")
        print("Any:   ", try value.evaluate(env, returnType: ValueCoercion()))
        print("Text:  ", try value.evaluate(env, returnType: TextCoercion()))   // -> "2" (Text)
        print("String:", try value.evaluate(env, returnType: StringCoercion())) // -> "2" (String)
        print("Int:   ", try value.evaluate(env, returnType: IntCoercion()))    // -> 2   (Int)
        print("Double:", try value.evaluate(env, returnType: DoubleCoercion())) // -> 2.0 (Double)
    }
    let command = Command("+", Text("2"), Text("3"))
    let result = try env.callProcedure(command, returnType: ValueCoercion(), commandScope: env)
    print("CMD:", command, "RES:", result) // -> `'+' {"2", "3"}` -> Text("5.0")
    
    print(try env.callProcedure(Command("nothing"), returnType: ValueCoercion(), commandScope: env)) // -> 'nothing'
    
    try env.store(Name("wibble")) // define a named "constant" (i.e. a name that evals to itself)
    print(try env.callProcedure(Command("wibble"), returnType: ValueCoercion(), commandScope: env)) // -> 'wibble'
    */
    
 //   print("EVALED:", try value.evaluate(env, returnType: ThunkCoercion(returnType: IntCoercion())))   // -> Thunk("2", IntCoercion)
 //   print("EVALED:", try value.evaluate(env, returnType: ThunkCoercion(returnType: IntCoercion())).evaluate(env, returnType: gValueCoercion)) // incorrect; currently returns Text
    
} catch { print("\n",error) }




