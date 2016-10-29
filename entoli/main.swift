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
source = " 3not b"  //   wrong:  produces  `"3". 'not' {'b'}.`, but `not` isn't a new word so should be treated as [unknown] unit suffix to `3` (the parser will warn this doesn't work right yet)
//source = " foo {a label:1, 2, c and not d, 4 + 6 = 10} "
//source = "4 + 6 * 10"         // correct:   `'+' {"4", '×' {"6", "10"}}.`
//source = "4 + 6 < 10"         // correct:   `'<' {'+' {"4", "6"}, "10"}.`
//source = "4 + 6 is after 10"  // correct:   `'is after' {'+' {"4", "6"}, "10"}.`
//source = "4 + 6 != 10"          // correct:   `'≠' {'+' {"4", "6"}, "10"}.`
//source = "0-123-7.34" // TO DO: hyphenated numbers currently split into separate negative numbers; should eventually be pattern matched as dates
//source = "forward 50 left 90 forward 50. repeat {4 (forward 50 left 90)}. repeat 4 [forward 50 left 90]" // note: `repeat 4 [...]` will parse as two exprs, so  it's important that code is rewritten to show this (e.g. by inserting comma between them)
//source = " repeat 4 do \n  forward 50 left 90  \n done"
//source = "forward 100 left turn, forward 40, left turn 45, forward 10. turn -90 forward 25"
//source = " \"foo\" is not before \"bar\" as case-insensitive text "

//source = " guess: random number {1 thru 10} as lazy. "

//source = " to foo bar {} do \n this, that, the other 42 \n done."

//source = " [a:1], (b:2), {c:3}, d:4, \"e\":5  "

//source = " (2 + 3 * 4 - 1.5) div 1" // -> Text("12")

//source = " [ 2 ,  4. 6 ] " //  EntoliScript([List(Text("2"), Text("4"), Text("6"))])

// EntoliScript([List(Text("2"), Name("foo"), Command(Name("bar"), Record()), (Command(Name("baz"), Record())))])
//source = " [2, foo, bar{}, (baz)] " // TO DO: problem: `foo` parses as Name but should eval as Command; OTOH, in a record it needs to eval as command OR name depending on use case, e.g. `foo {bar}` vs `to foo {param}`; also, lookahead is needed to determine if `as` operator is next (which in turn is problematic because `as` is not part of core parser so cannot be relied on to exist); TBH, even `foo{bar}` can't be converted to `foo{bar{}}` by parser as it has no way of knowing what `foo` proc wants to do with `bar`, e.g. it might want to treat it as a name, in which case user would have to parens it to have it eval as command [which hopefully returns a name] instead




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

let EVAL_TEST = 0

if EVAL_TEST != 0 {
    do {
        // initialize an environment
        let env = Scope()
        try loadLibrary(env)
        
        
     //   let script = "[3]"
        let script = " [(2 + 3 * 4 - 1.5), 3.14 div 1]"
      //  let script = " store {5, x}. x () " // note: empty expression group is equivalent to passing `{}` or `nothing`
    //    let script = " to foo {} 3 + 1. foo " // test native procedure definition (currently doesn't work as ParameterTypeCoercion is TBC)
        print("PARSE: \(script)")
        let value = try Parser(lexer: Lexer(code: script)).parseScript()
        print("=>", try value.evaluate(env, returnType: gAnythingCoercion))
    //    print(env)
        print("\n================================================\n")

        
        do {
            let value = Text("2")
            print("COERCE: \(value)")
            // test Entoli data evaluation
            print("Any:   ", try value.evaluate(env, returnType: gAnyValueCoercion))
            print("Text:  ", try value.evaluate(env, returnType: gTextCoercion))   // -> "2" (Text)
            // test Entoli->Swift data mapping
            print("String:", try value.evaluate(env, returnType: gStringCoercion)) // -> "2" (String)
            print("Int:   ", try value.evaluate(env, returnType: gIntCoercion))    // -> 2   (Int)
            print("Double:", try value.evaluate(env, returnType: gDoubleCoercion)) // -> 2.0 (Double)
            print("")
        }
        
        print("\n================================================\n")
        
        // test procedure call, `+ {2, 3}` (or `2 + 3` if operator syntax sugar is used)
        print("CALL:")
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
}



let EVAL_TEST2 = 1

if EVAL_TEST2 != 0 {
    do {
        // initialize an environment
        let env = Scope()
        try loadLibrary(env)
        
        //let script = "to add {} 2 + 2. add."
        //let script = "to add {} (1+5,3*7,2 + 2). add."
        //let script = "y: 2. to add {x} (x) + y. add 3."
        
        // TO DO: need to figure out exactly where a Name should be evaled as a Command instead of a Name (can't trust parser as the decision is context-sensitive, e.g. `x as name` will treat `x` as Name [basically a special-case for preventing `x` being evaled as Command in a command context], whereas `x as text` would treat `x` as Command; similarly, `{x:1}` will treat `x` as Name whereas `[x:1]` will treat it as Command, while `x:1` in a block will be treated as `store{1,x}`). Part of reworking evaluate->coerce->expand call chain.
        
        // TO DO: prob. best for Coercion.coerce() to be entry point, as coercions know more about context requirements than values do; only question is what values need special handling
        
        // Q. what are special coercions (i.e. that don't tell value to expand itself normally)? `NAME as procedure` (returns the named procedure as closure value), `EXPR as expression` (returns the expression[seq] without evaluating it [presumably] as thunk); DoNotEvaluate (in primitives, returns EXPR as-is, allowing proc to construct its own env in which to evaluate it)
        
        // Q. when an EXPR is thunked, it can be evaled by coercing to any [or any other type]; to keep it as a thunk (e.g. when passing as argument) need to re-coerce to `expression` each time
        
        // note: unlike kiwi, entoli [currently?] doesn't allow procs to inject slots into a thunk; e.g. a regexp proc that takes either text or expr as replacement value can't inject `matches` into the latter
        
        
   //     let script = "to add {x} 1 + x{}. add 3." // this works
        let script = "to add {x} 1 + x. add 3." // this fails because 'x' is a Name, which can't coerce to scalar
  //      let script = "to add {x} x + 1. add 3." // this parses correctly now (`+` in this context is obviously intended as an infix operator with `x` as its LH operand; previously it was being treated as prefix operator to `1`, causing it to parse as `x{+1}`)
    
        //  let script = " store {5, x}. x () " // note: empty expression group is equivalent to passing `{}` or `nothing`
        //    let script = " to foo {} 3 + 1. foo " // test native procedure definition (currently doesn't work as ParameterTypeCoercion is TBC)
        print("PARSE: \(script)\n")
        let value = try Parser(lexer: Lexer(code: script)).parseScript()
        print(value)
        print("\nEVAL SCRIPT:")
        print("=>", try value.evaluate(env, returnType: gAnythingCoercion))
        //    print(env)
        print("\n================================================\n")
        
    } catch { print("\nA TEST FAILED:",error) }
}


