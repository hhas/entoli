//
//  library.swift
//  entoli-run
//
//  Standard library
//  
//

// TO DO: developer jargon: an 'environment' ('env') = a chain of one or more Scopes? or should we just use 'scope' throughout, since 'scope' already [by recursive definition] = a scope with link to optional parent scope? 
//
// (note: prefer not to use the term 'activation record' or 'stack frame', since entoli uses the same Scope class for procedure activation records and module scopes, and anything else you might want to use Scope instances for [e.g. as the internal storage for JS-style 'Objects'... although given they only have a single parent delegate they're not ideal for that - NewtonScript had the right idea here - but it ought to be cheap enough to catch any errors when parent chain lookup fails and look in lexical scope next; or require explicit `CMD of me` for chain lookups, which also avoids the risk of lexical procs being masked by identically named procs appearing in a parent scope - something that Swift also gets wrong, though Python gets more or less right])


// TO DO: implement procedure overloading support: `-` op is both prefix and infix, so requires two procedures with same name and different signatures: {left,right} and {right}; need to think about how this is going to work, as current design only allows one proc per slot (which is probably wise as it keeps implementation simple for common case and can just use a 'multiproc' wrapper, although it also raises questions about how to deal with masking, since a proc in a child scope will normally 'replace' identically named proc in parent scope, whereas we only want to mask a specific signature; also bear in mind that a parent scope may also evolve over time, so it's not just simple case of `store` checking for same name in its parent scopes; nor can we append new ops to parent scope, as they shouldn't be visible outside of the scope in which they were defined, otherwise they're liable to create incredibly nasty and totally unpredictable and uncontrollable crosstalk between libraries, which won't like having their own parents suddenly mutate from under them). Simplest would be to require all overloads to be defined at once in same scope, using a variation of `do` that takes one or more `signature:expression` pairs (though even that gets fiddly as we've not yet figured out how `catch`, `returning`, etc should be attached to proc def); thus overridding name in a child scope would mask _all_ those definitions, not extend, which is a bit limiting but at least [hopefully] predictable; plus the overriding definition could still choose to delegate to its parent scope if no match is found, which brings back behavioral extension in a [hopefully] reliable and predictable way (TBH, delegating between environment scopes shouldn't really be any different to delegating between instance methods in class inheritance chain, and is something e.g. AppleScript already does out of the box thus avoiding any need for distinct and semi-overlapping but not quite interchangeable 'module' vs 'class/instance' types).



// note: since native names in entoli are already case-insensitive, any existing casing can be ignored; thus AOT compilation of entoli code should be able to convert those names to Swift identifiers by making them all-lowercase and converting words from space-separated to camelcase-separated. If the entoli name also contains characters that are not allowed in Swift identifiers, use `_0u...._` sequences (note that unicode standard character names should also work, and might be preferable if not too long themselves, while very common characters such as `&` could use a table of predefined substitutes, e.g. "_and_"); similarly, if generated name conflicts with a known/reserved Swift name then prefix/suffix with `__`


// TO DO: how to package this crap? (e.g. swift stdlib defines Process as enum with static properties, but frankly that smells)

// one thing to remember is that primitive funcs should be pure Swift logic - no glue code - so that compiler can emit Swift code just by chaining primitive funcs together when there is sufficient entoli type info to eliminate intermediate Value boxing and unboxing (Q. what about inlining and/or ability to output source code directly?)

// TO DO: how/where to merge proc's returnType with caller's required type (Q. how? making this a generic func screws up class wrapper; making return type Any might avoid the problem); might be best if Constraint.union() always returns a NativeSwiftCast, and have that return an opaque value that is easily unwrapped, e.g. if caller wants Array<Int> and func's return type is ListType(itemType:IntegerType()), we don't want to wrap and unwrap every single item

// note: one issue with library-defined operators is that scripts won't parse/format correctly unless those libraries are present (or at least their operator definitions, which should exist as part of the documentation; suggesting the problem might be ameliorated by library repository making that documentation available even when [e.g. commercial] libraries aren't; another option, of course, is to reduce scripts to command-only format for exchange, although that isn't good for readability; yet another option is to copy custom operator definitions to script's header for reference, thereby at least localizing the ugliness)

//  TO DO: need to think about how libraries might define their own Value subclasses (e.g. as opaque wrappers for external objects, e.g. GUI classes) and corresponding commands, although where possible libraries should stick to built-in types, using records to describe complex data structures (e.g. it might be that a record's values could be read-only to prevent modification outside of its associated commands; private state could similarly be hidden in annotations, although it should always be reconstructable since annotations are not guaranteed to persist, and two records that look identical should be interchangeable, regardless of how they were created; otherwise use an opaque value, which makes no such promises)


// TO DO: should `if` proc take a {test:BOOL-EXPR,action:ANY-EXPR} record, or a `BOOL-EXPR:ANY-EXPR` pair? (It'll probably need braced/parensed anyway, unless `if` is an auto-delimited prefix operator; if a record, the action expr can be written as either an atomic block within the record or as a postfixed do-block, i.e. `if BOOL-EXPR do ... done`; if a colon pair the colon must always be written. Might want to think about `to` notation here, which is still debating whether or not to be based on a colon pair, as consistency would simplify learning and usage.) [One concern with colon pairs is operator precedence, e.g. `:` should bind looser than arithmetic operators, but tighter than `of`, `else`, etc; can't help suspecting we'll end up wanting circular precedence, where A > B > C > A, which'll be a giant PITA to support/work reliably]


//**********************************************************************


extension PrimitiveProcedure { // convenience constructors for standard math operators // TO DO: some misgivings about extending PrimitiveProcedure rather than defining `makeArithmeticProcedure(name:function:)`, `makeComparisonProcedure(name:function:)` funcs
    
    static let leftFieldSignature         = (name: gLeftOperandKeyString, type: gScalarConstraint as Constraint)
    static let rightFieldSignature        = (name: gRightOperandKeyString, type: gScalarConstraint as Constraint)
    static let scalarInfixParameterType   = [leftFieldSignature, rightFieldSignature] // currently used by `ProcedureSignature()` in init() below
    static let scalarPrefixParameterType  = [leftFieldSignature] // currently unused
    static let scalarPostfixParameterType = [rightFieldSignature] // currently unused
    
    convenience init(name: String, scalarArithmeticOperator: @escaping ScalarArithmeticFunction) {
        self.init(name: name, function: scalarArithmeticOperator, returnType: gScalarConstraint)
    }
    
    convenience init(name: String, scalarComparisonOperator: @escaping ScalarComparisonFunction) {
        self.init(name: name, function: scalarComparisonOperator, returnType: gBoolConstraint)
    }
    
    convenience init<ReturnType>(name: String, function: @escaping ((Scalar, Scalar) throws -> ReturnType.SwiftType), returnType: ReturnType)
                                                                                        where ReturnType: Constraint, ReturnType: SwiftCast {
        let signature = ProcedureSignature(name: name, input: PrimitiveProcedure.scalarInfixParameterType, output: returnType)
        func wrapperFunction(_ arguments: [Value], commandScope: Scope, procedureScope: Scope) throws -> Value {
            var arguments = arguments
            let arg1 = try evalRecordField(in: &arguments, fieldStructure: (gLeftOperandKeyString, gScalarConstraint), commandScope: commandScope)
            let arg2 = try evalRecordField(in: &arguments, fieldStructure: (gRightOperandKeyString, gScalarConstraint), commandScope: commandScope)
            if arguments.count > 0 { throw BadArgument(description: "Too many arguments(s): \(arguments)") }
            return try returnType.wrap(try function(arg1, arg2), env: procedureScope)
        }
        self.init(signature: signature, procScope: .none, function: wrapperFunction)
    }
}


//**********************************************************************
// the following proc_* tuple describes the procedure interface for a single primitive func_*, and needs to be recognizable and parseable to glue code generator
//
// (note: this tuple doesn't have a standard type as parameter tuple does not have fixed length/type, and a variable-length structure such as Array doesn't provide the static type info required by evalRecordField() generic, which needs to know the *exact* Constraint class in order to determine its correct return type; thus we can't type the other fields either, which is not ideal as it'd allow problems to be spotted during authoring rather than glue generation; still, there's a good chance all this implementation will radically change in future anyway, so no point sweating details too much right now)

// TO DO: should value's type be gDoNotEvaluate, allowing func_storeValue to determine if it is an `as` command and use that as slot's type if mutable? Alternatively, define a TypedValueConstraint that unpacks to `(Value,Constraint)` tuple? (Yet another option is just to let the `as` operator annotate the value as it expands and coerces it, and then check that any time the slot is mutated. (There is also the question of how much attention the slot should pay to the value's existing type tags versus a literal `as` operator declared within the assignment, especially since the same value may have different tags depending on prior usage - e.g. a Text value might or might not have an 'integer' tag, which may be more specific than the user intends the slot to be.)

// TO DO: still some misgivings over order of `value` and `name[d]`, as it's opposite to the order used in `NAME:VALUE` shortcuts used in blocks

let storeValue_name   = "store"
let storeValue_input  = (value: ("value", gAnyValueConstraint), // TO DO: still a question over how to specify constraint on slot; one option would be for `value`'s type to be `ConstrainedValue`, which would take a `VALUE as TYPE` expression and dissect that (caveat: if there's no operators installed then it'd need to take `as{VALUE,TYPE}`)
                          name: ("named", gKeyStringConstraint)) // TO DO: rename "in"? e.g. `store {value: 2, in: x}`
let storeValue_output = gNoResult // (since the primitive func doesn't return a result, its generated wrapper will return `nothing` which should be passed through as-is; gNoResult simply provides a human-readable description that will appear in documentation)
let storeValue_env    = PrimitiveProcedure.ProcScope.commandScope

func storeValue(env: Scope, name: String, value: Value) throws {
    try env.store(name, value: value) // TO DO: `editable` flag, etc; Q. could type, flags, docs, etc. all be provided by `as clause` and annotations? if so, another possibility would be to define `store` as a unary operator that takes a named pair as its RH operand, c.f. AS's `property NAME : VALUE`, but able to work in any evaluation context (eliminating the need for >1 assignment syntax)
}


// TO DO: what about describing errors [as part of result]?
// TO DO: proc metadata (documentation, categorization, etc)
// TO DO: operator definition (optional)
// TO DO: should there be standard for declaring aliases?


//**********************************************************************


// TO DO: note that primitive funcs will need to be declared `public` so that entoli compiler can compose them directly, avoiding need to go through wrap/unwrap each time when Swift values can be passed directly. (Note: even more aggressive optimisation may be achieved when a primitive func provides a Swift code macro/SIL/whatever we end up using; e.g. entoli arithmetic/comparison commands should compile down to `A + B`, `A > B` etc when A.Type==B.Type [or variants thereof when A.Type!=B.Type, e.g. `(try A.toSwift() as Int) > 42`].)
//
// OTOH, metadata consts should prob. all be private, as any examination should be done via metadata APIs, or via generated metadata file included in script bundle (which will allow user docs to be generated even in absence of entoli runtime; e.g. a web-based library repository will want the metadata to maximize flexability, e.g. for providing online searchability, and won't want to load the module since 1. that requires an entoli runtime, and 2. requires all dynamically-linked Swift dependencies to be present as well - i.e. an administrative and security nightmare); the alternative would be for devs to write the metadata/bridging as entoli code which could be bundled as-is; the module code generator would then evaluate that in a restricted, dedicated entoli env to generate the Swift consts, and only the primitive proc would be written in Swift (TBH, neither approach is ideal, but that's what you get for using a non-extensible macro-less language like Swift.)


// TO DO: there might also be an argument for separating proc creation from proc storage, in which case a `to` operator would just take the proc value as its RH operand (currently to create a lambda value one would store the named proc first, then use `NAME as procedure` to retrieve it as a portable value); the `to` operator also has the advantage of being more forgiving on syntax whereas having a distinct proc value would either require a custom operator or else a standard colon pair (the former is sticky as we're running out of symbols to write it, and the latter sticky as it always requires a colon and will parse as an expr sequence if that's omitted)

// TO DO: also accept gNameConstraint/PairType(gNameConstraint,gTypeConstraint) as `input`, in which case the entire input [record?] is bound to that name; this'd be equivalent to using `foo(*args, **kwargs)` in Python (this won't be available on `to` operator though as `to PROC(PARAM)…` syntax will be a common typo that auto-correct will have to change to `to PROC{PARAM}…` to prevent confusion, and while writing it as `to 'PROC' 'PARAM'…` would avoid parser ambiguity it wouldn't be clear why parens can't disambiguate `PARAM` too)


// TO DO: problem if tuple fields are unnamed: if there's only one item then outer parens will look like a group, not a tuple; therefore, prob. best to require explicit labels; Q. is there any way to type this crap as a variable-length tuple


func defineProcedure(env: Scope, name: Name, input: ParameterType, output: ReturnType, body: Value) throws {
    try env.store(NativeProcedure(signature: ProcedureSignature(name: name, input: input, output: output), body: body))
}

let defineProcedure_name   = "to"
let defineProcedure_input  = (name: ("name",   gNameConstraint),
                             input: ("input",  gParameterTypeConstraint),
                            output: ("output", gReturnTypeConstraint),
                            action: ("action", gDoNotEvaluate)) // don't thunk expression as it'll be evaled with a new sub-env containing param
let defineProcedure_output = gNoResult
let defineProcedure_env    = PrimitiveProcedure.ProcScope.commandScope // `to` command's scope = proc's lexical scope



// Q. when will a primitive procedure need to use `.procedureScope` (i.e. the module space)? in general a primitive module would just use Swift vars to hold any private shared state; one use case would be when cloning an existing scope to create an AS-style object, in which case the original scope's procs would need to adopt the new scope as their parent (note to self: don't look at defineProcedure for reference here; it's a special case amongst special cases; could really do with some extra, general-purpose, procs to explore here)


//**********************************************************************
// END DEVELOPER-WRITTEN CODE
//**********************************************************************

//**********************************************************************
// BEGIN ENTOLI-BUILD-GENERATED CODE
//**********************************************************************
// each primitive func gets wrapped in a code-generated FUNCNAME_proc func that unpacks and repacks its arguments and results/errors

// argument unpack consumes the arguments list as a stack, popping off record fields as it matches them up to the corresponding signature fields

// TO DO: include entire proc definition in ProcedureError, not just name? (this'd require looking up name now in procedureScope, in case scope's state subsequently mutates, although if slot is locked - which it usually should be for procs - that shouldn't happen; alternatively, defining PrimitiveProcedure instance as top-level generated `let` should make it directly available) [TBH, probably sufficient to have its name and definition scope, as long as users understand that may go stale if the scope is further modified between the time the error was raised and the time it was introspected; the rationale being that it's a live system, so if user wants to introspect it for debugging purposes, it's up to her to halt the world/not to mutate it in the meantime - live debugging being inherently live and therefore inherently unsafe]


private func storeValue_proc(_ arguments: [Value], commandScope: Scope, procedureScope: Scope) throws -> Value {
    var arguments = arguments
    // next 2 lines are generated; note that the order of argument record's fields may or may not be same as order of primitive func's parameters (in this case they're not)
    let value_arg = try evalRecordField(in: &arguments, fieldStructure: storeValue_input.0, commandScope: commandScope)
    let name_arg = try evalRecordField(in: &arguments, fieldStructure: storeValue_input.1, commandScope: commandScope)
    if arguments.count > 0 { throw BadArgument(description: "Too many arguments(s): \(arguments)") }
    do {
        try storeValue(env: commandScope, name: name_arg, value: value_arg) // generated line
    } catch {
        throw ProcedureError(name: storeValue_name, arguments: arguments,
                             commandScope: commandScope, procedureScope: procedureScope, originalError: error)
    }
    return gNullValue // generated line (note: if the primitive func returns a result, it'll be wrapped as a Value here using `storeValue_output.wrap()`; if not, the gNullValue constant is returned)
}

private let storeValue_signature = ProcedureSignature(name: storeValue_name,
                                                     input: [(storeValue_input.0.0, storeValue_input.0.1),
                                                             (storeValue_input.1.0, storeValue_input.1.1)],
                                                    output: storeValue_output as Constraint)

//

private func defineProcedure_proc(_ arguments: [Value], commandScope: Scope, procedureScope: Scope) throws -> Value {
    var arguments = arguments
    // next 4 lines are generated
    let name_arg = try evalRecordField(in: &arguments, fieldStructure: defineProcedure_input.0, commandScope: commandScope)
    let input_arg = try evalRecordField(in: &arguments, fieldStructure: defineProcedure_input.1, commandScope: commandScope)
    let output_arg = try evalRecordField(in: &arguments, fieldStructure: defineProcedure_input.2, commandScope: commandScope)
    let action_arg = try evalRecordField(in: &arguments, fieldStructure: defineProcedure_input.3, commandScope: commandScope)
    if arguments.count > 0 { throw BadArgument(description: "Too many arguments(s): \(arguments)") }
    do {
        try defineProcedure(env: commandScope, name: name_arg, input: input_arg, output: output_arg, body: action_arg) // generated line
    } catch {
        throw ProcedureError(name: defineProcedure_name, arguments: arguments, // TO DO: include entire proc definition, not just name? (this'd require looking up name now in procedureScope, in case scope's state subsequently mutates, although if slot is locked - which it usually should be for procs - that shouldn't happen; alternatively, defining PrimitiveProcedure instance as top-level generated `let` should make it directly available)
            commandScope: commandScope, procedureScope: procedureScope, originalError: error)
    }
    return gNullValue
}

// code-generated signature (annoying we need this, but the dev-provided PROCNAME_input constant has to use a tuple, not an array, to allow static type-checking; conversely, the signature needs something it can iterate over)

private let defineProcedure_signature = ProcedureSignature(name: defineProcedure_name,
                                                           input: [(defineProcedure_input.0.0, defineProcedure_input.0.1), // extra tediousness
                                                                   (defineProcedure_input.1.0, defineProcedure_input.1.1),
                                                                   (defineProcedure_input.2.0, defineProcedure_input.2.1),
                                                                   (defineProcedure_input.3.0, defineProcedure_input.3.1)],
                                                         output: defineProcedure_output as Constraint)



//**********************************************************************
// each library has a loadLibrary() func that adds its procs to the given environment scope

func loadLibrary(_ env: Scope) throws {
    try env.store(PrimitiveProcedure(signature: storeValue_signature, procScope: storeValue_env, function: storeValue_proc))
    try env.store(PrimitiveProcedure(signature: defineProcedure_signature, procScope: defineProcedure_env, function: defineProcedure_proc))
    
    // TO DO: how to do overloaded procs, e.g. `'+' {right:}` and `'+' {left:,right:}`? (note that operator table already generates both, though only the latter will currently evaluate; for now, using +/- prefix operators will always throw a 'missing argument' error because there aren't yet `'+'{right:}`/`'-'{right:}` proc installed for those) IIRC, one idea was to install first proc as normal; then, if an overload is also installed (either in same load sequence or, slot permissions-willing, installed later) to swap out the original proc 
    
    try env.store(PrimitiveProcedure(name: "+",   scalarArithmeticOperator: (+)))
    try env.store(PrimitiveProcedure(name: "-",   scalarArithmeticOperator: (-)))
    try env.store(PrimitiveProcedure(name: "×",   scalarArithmeticOperator: (*)))
    try env.store(PrimitiveProcedure(name: "÷",   scalarArithmeticOperator: (/)))
//    try env.store(PrimitiveProcedure(name: "mod", scalarArithmeticOperator: (%))) TO DO: truncatingRemainder
    try env.store(PrimitiveProcedure(name: "div", scalarArithmeticOperator: (integerDivision)))
    try env.store(PrimitiveProcedure(name: "^",   scalarArithmeticOperator: (pow)))
    
    try env.store(PrimitiveProcedure(name: "<", scalarComparisonOperator: (<)))
    try env.store(PrimitiveProcedure(name: "≤", scalarComparisonOperator: (<=)))
    try env.store(PrimitiveProcedure(name: "=", scalarComparisonOperator: (==)))
    try env.store(PrimitiveProcedure(name: "≠", scalarComparisonOperator: (!=)))
    try env.store(PrimitiveProcedure(name: ">", scalarComparisonOperator: (>)))
    try env.store(PrimitiveProcedure(name: "≥", scalarComparisonOperator: (>=)))
    // TO DO: min, max procs that take list of one or more numbers
    
    let pi = Text("3.14159265359")
    pi.annotations.append(Scalar(3.14159265359))
    try env.store("π", value: pi)
}

