//
//  main.swift
//  entoli



// TO DO: following isn't working quite right yet: the `run script{"yes"}` command isn't appearing in parsed result
//let source = "hello is  equal to \tthat  {user name :1, 'smith', to:A and B, 99:nope} \n run script \"yes\" foo: bar: baz"

//let source = " 'one' 'two' 'three' "


//let source = " (FOO) is not equal. to BAR " // TO DO: this period is causing double period to show when displayed, presumably because it remains attached to first expr as postfix op; the group/script then inserts its own period delimiter when displaying


//let code: ScriptChars = "3.2e+5.1".characters; print(readNumericChars(code, start: code.startIndex))




//let source = "  bob is not smith   !=x   1   /   2   +   4  . " //. 2*2. 3 - 3. 4-4. "


//let source = " 1 / 2 + 4  "  //   wrong:  `"1". '/' {"2"}. '+' {"4"}.`

let source = " a / b + c "      //  really wrong:  `'a / b + c'.`


let lexer = Lexer(code: source)

/*
var i = 0
repeat {
    i+=1
    print("==>TOKEN:", lexer.currentToken)
    print("__________________________________________________________________\n")
    lexer.advance(true, ignoreVocabulary: false)
} while lexer.currentToken.type != gEndOfCodeToken.type //&& i<4

*/

let p = Parser(lexer: lexer)


do {
    let result = try p.parse()
    print("\n\n================================================\n", result)
} catch {
    print("\n\n================================================\nERROR:", error)
}
/*
*/




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


