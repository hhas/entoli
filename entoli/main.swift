//
//  main.swift
//  entoli



let source = "(hello is  equal to \tthat  {user name :1, 'smith', to:A and B, 99:nope} \n run script \"yes\" foo: bar: baz)"

//let source = " 'one' 'two' 'three' "


//let source = " (FOO) is not equal to BAR "


var lexer = Lexer(code: source)


let p = Parser(lexer: lexer)


do {
    print(try p.parse())
} catch {
    print(error)
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


