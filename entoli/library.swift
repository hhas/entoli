//
//  library.swift
//  entoli-run
//
//  Standard library
//  
//

// TO DO: implement procedure overloading support: `-` op is both prefix and infix, so requires two procedures with same name and different signatures: {left,right} and {right}; need to think about how this is going to work, as current design only allows one proc per slot (which is probably wise as it keeps implementation simple for common case and can just use a 'multiproc' wrapper, although it also raises questions about how to deal with masking, since a proc in a child scope will normally 'replace' identically named proc in parent scope, whereas we only want to mask a specific signature; also bear in mind that a parent scope may also evolve over time, so it's not just simple case of `store` checking for same name in its parent scopes; nor can we append new ops to parent scope, as they shouldn't be visible outside of the scope in which they were defined, otherwise they're liable to create incredibly nasty and totally unpredictable and uncontrollable crosstalk between libraries, which won't like having their own parents suddenly mutate from under them). Simplest would be to require all overloads to be defined at once in same scope, using a variation of `do` that takes one or more `signature:expression` pairs (though even that gets fiddly as we've not yet figured out how `catch`, `returning`, etc should be attached to proc def); thus overridding name in a child scope would mask _all_ those definitions, not extend, which is a bit limiting but at least [hopefully] predictable; plus the overriding definition could still choose to delegate to its parent scope if no match is found, which brings back behavioral extension in a [hopefully] reliable and predictable way (TBH, delegating between environment scopes shouldn't really be any different to delegating between instance methods in class inheritance chain, and is something e.g. AppleScript already does out of the box thus avoiding any need for distinct and semi-overlapping but not quite interchangeable 'module' vs 'class/instance' types).



// note: since native names in entoli are already case-insensitive, any existing casing can be ignored; thus AOT compilation of entoli code should be able to convert those names to Swift identifiers by making them all-lowercase and converting words from space-separated to camelcase-separated. If the entoli name also contains characters that are not allowed in Swift identifiers, use `_0u...._` sequences (note that unicode standard character names should also work, and might be preferable if not too long themselves, while very common characters such as `&` could use a table of predefined substitutes, e.g. "_and_"); similarly, if generated name conflicts with a known/reserved Swift name then prefix/suffix with `__`


// TO DO: how to package this crap? (e.g. swift stdlib defines Process as enum with static properties, but frankly that smells)

// one thing to remember is that primitive funcs should be pure Swift logic - no glue code - so that compiler can emit Swift code just by chaining primitive funcs together when there is sufficient entoli type info to eliminate intermediate Value boxing and unboxing (Q. what about inlining and/or ability to output source code directly?)

// TO DO: how/where to merge proc's returnType with caller's required type (Q. how? making this a generic func screws up class wrapper; making return type Any might avoid the problem); might be best if Coercion.union() always returns a NativeSwiftCast, and have that return an opaque value that is easily unwrapped, e.g. if caller wants Array<Int> and func's return type is ListType(itemType:IntegerType()), we don't want to wrap and unwrap every single item

// note: one issue with library-defined operators is that scripts won't parse/format correctly unless those libraries are present (or at least their operator definitions, which should exist as part of the documentation; suggesting the problem might be ameliorated by library repository making that documentation available even when [e.g. commercial] libraries aren't; another option, of course, is to reduce scripts to command-only format for exchange, although that isn't good for readability; yet another option is to copy custom operator definitions to script's header for reference, thereby at least localizing the ugliness)

//  TO DO: need to think about how libraries might define their own Value subclasses (e.g. as opaque wrappers for external objects, e.g. GUI classes) and corresponding commands, although where possible libraries should stick to built-in types, using records to describe complex data structures (e.g. it might be that a record's values could be read-only to prevent modification outside of its associated commands; private state could similarly be hidden in annotations, although it should always be reconstructable since annotations are not guaranteed to persist, and two records that look identical should be interchangeable, regardless of how they were created; otherwise use an opaque value, which makes no such promises)


//**********************************************************************
// TEMP: library support functions; copy-pasting is due to limitations of swift generics; also, the argument and result types also really want to be generic parameters, but type checker throws a strop which is greatly annoying as all operator function definitions could otherwise be wrapped using just three funcs (prefix,infix,postfix); one option might be to try parameterizing class with common signature types and see if individual methods are then accepted



// worth moving these to PrimitiveProcedure as `init(name:Name,scalarArithmeticFunction:ScalarArithmeticFunction)`, etc?

let leftScalarOperand = (gLeftOperandName, gScalarCoercion)
let rightScalarOperand = (gRightOperandName, gScalarCoercion)

func wrapScalarArithmeticOperator(_ function: @escaping ScalarArithmeticFunction) -> PrimitiveProcedure.FunctionWrapper {
    return { (arguments: [Value], commandScope: Scope, procedureScope: Scope) throws -> Value in
        var arguments = arguments
        let arg1 = try evalRecordField(&arguments, fieldStructure: leftScalarOperand, commandScope: commandScope)
        let arg2 = try evalRecordField(&arguments, fieldStructure: rightScalarOperand, commandScope: commandScope)
        if arguments.count > 0 { throw BadArgument(description: "Too many arguments(s): \(arguments)") }
        return try gScalarCoercion.wrap(try function(arg1, arg2), env: procedureScope)
    }
}

func wrapScalarComparisonOperator(_ function: @escaping ScalarComparisonFunction) -> PrimitiveProcedure.FunctionWrapper {
    return { (arguments: [Value], commandScope: Scope, procedureScope: Scope) throws -> Value in
        var arguments = arguments
        let arg1 = try evalRecordField(&arguments, fieldStructure: leftScalarOperand, commandScope: commandScope)
        let arg2 = try evalRecordField(&arguments, fieldStructure: rightScalarOperand, commandScope: commandScope)
        if arguments.count > 0 { throw BadArgument(description: "Too many arguments(s): \(arguments)") }
        return try gBoolCoercion.wrap(try function(arg1, arg2), env: procedureScope)
    }
}



//**********************************************************************
// the following proc_* tuple describes the procedure interface for a single primitive func_*, and needs to be recognizable and parseable to glue code generator
//
// (note: this tuple doesn't have a standard type as parameter tuple does not have fixed length/type, and a variable-length structure such as Array doesn't provide the static type info required by evalRecordField() generic, which needs to know the *exact* Coercion class in order to determine its correct return type; thus we can't type the other fields either, which is not ideal as it'd allow problems to be spotted during authoring rather than glue generation; still, there's a good chance all this implementation will radically change in future anyway, so no point sweating details too much right now)


private func func_storeValue(_ env: Scope, name: String, value: Value) throws {
    try env.store(name, value: value) // TO DO: `editable` flag, etc; Q. could type, flags, docs, etc. all be provided by `as clause` and annotations? if so, another possibility would be to define `store` as a unary operator that takes a named pair as its RH operand, c.f. AS's `property NAME : VALUE`, but able to work in any evaluation context (eliminating the need for >1 assignment syntax)
}

private let proc_storeValue = (name: "store",
                               // TO DO: should value's type be gDoNotEvaluate, allowing func_storeValue to determine if it is an `as` command and use that as slot's type if mutable? Alternatively, define a TypedValueCoercion that unpacks to `(Value,Coercion)` tuple? (Yet another option is just to let the `as` operator annotate the value as it expands and coerces it, and then check that any time the slot is mutated. (There is also the question of how much attention the slot should pay to the value's existing type tags versus a literal `as` operator declared within the assignment, especially since the same value may have different tags depending on prior usage - e.g. a Text value might or might not have an 'integer' tag, which may be more specific than the user intends the slot to be.)
                               parameter: (
                                    value: (Name("value"), gAnyValueCoercion),
                                    name:  (Name("named"), gNameKeyStringCoercion)), // TO DO: rename "in"? e.g. `store {value: 2, in: x}`
                               result: gNoResult, // since the primitive func doesn't return a result, its generated wrapper will return `nothing` which should be passed through as-is; gNoResult simply provides a human-readable description that will appear in documentation
                               procScope: PrimitiveProcedure.ProcScope.commandScope,
                               function: func_storeValue)



// TO DO: what about describing errors [as part of result]?
// TO DO: proc metadata (documentation, categorization, etc)
// TO DO: operator definition (optional)
// TO DO: should there be standard for declaring aliases?


//**********************************************************************


private func func_defineProcedure(_ env: Scope, name: Name, using: ParameterType, returning: ReturnType, body: Value) throws {
    try env.store(NativeProcedure(signature: ProcedureSignature(name: name, parameterType: using, returnType: returning), body: body))
}

private let proc_defineProcedure = (name: "to", // TO DO: decide proc name (currently it's the same as the operator, but there might be an argument for a long descriptive name here, e.g. `define procedure`); BTW, there might also be an argument for separating proc creation from proc storage, in which case a `to` operator would just take the proc value as its RH operand (currently to create a lambda value one would store the named proc first, then use `NAME as procedure` to retrieve it as a portable value); the `to` operator also has the advantage of being more forgiving on syntax whereas having a distinct proc value would either require a custom operator or else a standard colon pair (the former is sticky as we're running out of symbols to write it, and the latter sticky as it always requires a colon and will parse as an expr sequence if that's omitted)
                                    parameter: (
                                        name:      (Name("name"), gNameCoercion),
                                        using:     (Name("input"), gParameterTypeCoercion), // TO DO: also accept gNameCoercion/PairType(gNameCoercion,gTypeCoercion), in which case the entire input [record?] is bound to that name (this won't be available on `to` operator though as `to PROC(PARAM)…` syntax will be a common typo that auto-correct will have to change to `to PROC{PARAM}…` to prevent confusion, and while writing it as `to 'PROC' 'PARAM'…` would avoid parser ambiguity it wouldn't be clear why parens can't disambiguate `PARAM` too)
                                        returning: (Name("output"), gReturnTypeCoercion),
                                        body:      (Name("body"), gDoNotEvaluate) // don't thunk expression here as it'll be evaled with a new sub-env containing params // TO DO: what to name this arg? e.g. `body`, `code`, `expression`, `action`?
                                    ),
                                    result: gNoResult,
                                    procScope: PrimitiveProcedure.ProcScope.commandScope, // `to` command's scope = proc's lexical scope
                                    function: func_defineProcedure)


//**********************************************************************
// generated glue code
// each primitive func gets wrapped in a generated func that unpacks and repacks its arguments and results/errors


private func call_storeValue(_ arguments: [Value], commandScope: Scope, procedureScope: Scope) throws -> Value {
    var arguments = arguments
    // next 2 lines are generated
    let arg_value = try evalRecordField(&arguments, fieldStructure: proc_storeValue.parameter.0, commandScope: commandScope)
    let arg_name = try evalRecordField(&arguments, fieldStructure: proc_storeValue.parameter.1, commandScope: commandScope)
    if arguments.count > 0 { throw BadArgument(description: "Too many arguments(s): \(arguments)") }
    do {
        try proc_storeValue.function(commandScope, arg_name, arg_value) // generated line
    } catch {
        throw ProcedureError(name: proc_storeValue.name, arguments: arguments, // TO DO: include entire proc definition, not just name? (this'd require looking up name now in procedureScope, in case scope's state subsequently mutates, although if slot is locked - which it usually should be for procs - that shouldn't happen; alternatively, defining PrimitiveProcedure instance as top-level generated `let` should make it directly available)
                             commandScope: commandScope, procedureScope: procedureScope, originalError: error)
    }
    return gNullValue
}


private func call_defineProcedure(_ arguments: [Value], commandScope: Scope, procedureScope: Scope) throws -> Value {
    var arguments = arguments
    // next 4 lines are generated
    let arg_name = try evalRecordField(&arguments, fieldStructure: proc_defineProcedure.parameter.0, commandScope: commandScope)
    let arg_parameter = try evalRecordField(&arguments, fieldStructure: proc_defineProcedure.parameter.1, commandScope: commandScope)
    let arg_result = try evalRecordField(&arguments, fieldStructure: proc_defineProcedure.parameter.2, commandScope: commandScope)
    let arg_body = try evalRecordField(&arguments, fieldStructure: proc_defineProcedure.parameter.3, commandScope: commandScope)
    if arguments.count > 0 { throw BadArgument(description: "Too many arguments(s): \(arguments)") }
    do {
        try proc_defineProcedure.function(commandScope, arg_name, arg_parameter, arg_result, arg_body) // generated line
    } catch {
        throw ProcedureError(name: proc_defineProcedure.name, arguments: arguments, // TO DO: include entire proc definition, not just name? (this'd require looking up name now in procedureScope, in case scope's state subsequently mutates, although if slot is locked - which it usually should be for procs - that shouldn't happen; alternatively, defining PrimitiveProcedure instance as top-level generated `let` should make it directly available)
            commandScope: commandScope, procedureScope: procedureScope, originalError: error)
    }
    return gNullValue

}



//**********************************************************************
// generated glue code
// each library has a loadLibrary() func that adds its procs to the given environment scope

func loadLibrary(_ env: Scope) throws {
    try env.store(PrimitiveProcedure(name: proc_storeValue.name,
                                     type: (RecordSignature(
                                                FieldSignature(proc_storeValue.parameter.value.0, proc_storeValue.parameter.value.1),
                                                FieldSignature(proc_storeValue.parameter.name.0, proc_storeValue.parameter.name.1)),
                                            proc_storeValue.result),
                                     procScope: proc_storeValue.procScope, function: call_storeValue))
    try env.store(PrimitiveProcedure(name: proc_defineProcedure.name,
                                     type: (RecordSignature(
                                                FieldSignature(proc_defineProcedure.parameter.name.0, proc_defineProcedure.parameter.name.1),
                                                FieldSignature(proc_defineProcedure.parameter.using.0, proc_defineProcedure.parameter.using.1),
                                                FieldSignature(proc_defineProcedure.parameter.returning.0, proc_defineProcedure.parameter.returning.1),
                                                FieldSignature(proc_defineProcedure.parameter.body.0, proc_defineProcedure.parameter.body.1)),
                                            proc_defineProcedure.result),
                                     procScope: proc_defineProcedure.procScope, function: call_defineProcedure))
    
    // TO DO: how to do overloaded procs, e.g. `'+' {right:}` and `'+' {left:,right:}`? (note that operator table already generates both, though only the latter will currently evaluate; for now, using +/- prefix operators will always throw a 'missing argument' error because there aren't yet `'+'{right:}`/`'-'{right:}` proc installed for those) IIRC, one idea was to install first proc as normal; then, if an overload is also installed (either in same load sequence or, slot permissions-willing, installed later) to swap out the original proc 
    
    try env.store(PrimitiveProcedure(name: "+",   scalarArithmeticOperatorFunction: (+)))
    try env.store(PrimitiveProcedure(name: "-",   scalarArithmeticOperatorFunction: (-)))
    try env.store(PrimitiveProcedure(name: "×",   scalarArithmeticOperatorFunction: (*)))
    try env.store(PrimitiveProcedure(name: "÷",   scalarArithmeticOperatorFunction: (/)))
//    try env.store(PrimitiveProcedure(name: "mod", scalarArithmeticOperatorFunction: (%))) TO DO: truncatingRemainder
    try env.store(PrimitiveProcedure(name: "div", scalarArithmeticOperatorFunction: (integerDivision)))
    try env.store(PrimitiveProcedure(name: "^",   scalarArithmeticOperatorFunction: (pow)))
    
    try env.store(PrimitiveProcedure(name: "<", scalarComparisonOperatorFunction: (<)))
    try env.store(PrimitiveProcedure(name: "≤", scalarComparisonOperatorFunction: (<=)))
    try env.store(PrimitiveProcedure(name: "=", scalarComparisonOperatorFunction: (==)))
    try env.store(PrimitiveProcedure(name: "≠", scalarComparisonOperatorFunction: (!=)))
    try env.store(PrimitiveProcedure(name: ">", scalarComparisonOperatorFunction: (>)))
    try env.store(PrimitiveProcedure(name: "≥", scalarComparisonOperatorFunction: (>=)))
    // TO DO: min, max procs that take list of one or more numbers
    
    let pi = Text("3.14159265359")
    pi.annotations.append(Scalar(3.14159265359))
    try env.store("π", value: pi)
}

