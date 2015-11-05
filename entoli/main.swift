//
//  main.swift
//  entoli



// TO DO: following isn't working quite right yet: the `run script{"yes"}` command isn't appearing in parsed result
//let source = "hello is  equal to \tthat  {user name :1, 'smith', to:A and B, 99:nope} \n run script \"yes\" foo: bar: baz"

//let source = " 'one' 'two' 'three' "


//let source = " (FOO) is not equal. to BAR " // TO DO: this period is causing double period to show when displayed, presumably because it remains attached to first expr as postfix op; the group/script then inserts its own period delimiter when displaying

let source = "  bob   !=   1   /   2   +   4  . " //. 2*2. 3 - 3. 4-4. "


let lexer = Lexer(code: source, keywordOperators: StandardOperatorPhrasesTable)

/*
var i = 0
repeat {
    i+=1
    print("==>TOKEN:", lexer.currentToken, "\n----------------------------------------------------------\n")
    lexer.advance(ignoreVocabulary: false)
} while lexer.currentToken != nil //&& i<4
*/


let p = Parser(lexer: lexer)


do {
    let result = try p.parse()
    print("\n\n================================================\n", result)
} catch {
    print("\n\n================================================\nERROR:", error)
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


