//
//  main.swift
//  entoli



// TO DO: following isn't working quite right yet: the `run script{"yes"}` command isn't appearing in parsed result
//let source = "hello is  equal to \tthat  {user name :1, 'smith', to:A and B, 99:nope} \n run script \"yes\" foo: bar: baz"

//let source = " 'one' 'two' 'three' "


//let source = " (FOO) is not equal. to BAR " // TO DO: this period is causing double period to show when displayed, presumably because it remains attached to first expr as postfix op; the group/script then inserts its own period delimiter when displaying


//let code: ScriptChars = "3.2e+5.1".characters; print(readNumericWord(code, start: code.startIndex))




//let source = "  bob is not smith   !=x   1   /   2   +   4  . " //. 2*2. 3 - 3. 4-4. "


//let source = "a*b"  //   wrong:  `parseAtom() encountered an infix operator`

//let source = "a and b"

//let source = " 3*4 "  //   wrong:  `3*4` is being read as numeric, not operator


//  `'foo' {'a label': "1", "2", 'and' {'c', 'not' {'d'}, '+' {"4", "6"}, '=' {"10"}}.` // screwups: `=` isn't binding LH (off-by-one in lexer? compare `==`)
//let source = " foo {a label:1, 2, c and not d, 4 + 6 = 10} "

//let source = "4 + 6 * 10" // correct: `'+' {"4", '×' {"6", "10"}}.`
//let source = "4 + 6 < 10" // correct:  `'<' {'+' {"4", "6"}, "10"}.`
let source = "6 is after 10" // WRONG:  `'+' {"4", "6"}. '!=' {"10"}.`4 + 

// let source = "0-123-7.34" // TO DO: hyphenated numbers currently split into separate negative numbers; should eventually be pattern matched as dates

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


