//
//  main.swift
//  entoli


let source: String

// TO DO: following isn't working quite right yet: the `run script{"yes"}` command isn't appearing in parsed result
//let source = "hello is  equal to \tthat  {user name :1, 'smith', to:A and B, 99:nope} \n run script \"yes\" foo: bar: baz"

//source = " 'one' 'two' 'three' "


//source = " (FOO) is not equal to BAR "   // correct:  `'is not' {('FOO'), 'BAR'}.` (`is not equal to` is alias for `is not`)

// obsolete: //source = " (FOO) is not same. as BAR " // correct:  `parseAtom() encountered an infix operator: "as"`

source = " (FOO) is not equal. to BAR " // 

//source = " 4 is equal to not test 3" // correct:  `'is' {"4", 'not' {'test' {"3"}}}.`


//let code: ScriptChars = "3.2e+5.1".characters; print(readNumericWord(code, start: code.startIndex))




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


let test2 = 1

if test2 != 0 {
    let p = Parser(lexer: lexer)
    do {
        let result = try p.parse()
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


