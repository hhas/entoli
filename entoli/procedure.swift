//
//  procedure.swift
//  entoli-run
//
//
//

// TO DO: how practical for procs to declare they're idempotent? this'd be particularly useful for ProxyCoercions as it'd allow them to memoize the constructed Coercion on first use, rather than reconstructing it every time it's applied; problem is native procs don't know if they're idempotent as commands within their body are late-bound; also bear in mind that type args/operands can't be evaled until scope is fully populated by top-level exprs being evaled, but `to` commands need to eval param types in order to add themselves to scope (thus their full initialization really wants to be deferred until after the scope is fully populated)


//**********************************************************************


// note: primitive proc glue should code-generate one unpackArgument() call for each parameter in sig
// TO DO: this func should probably be generalized to work on any record, allowing it to be applied by coercion handlers as well (note: would probably be easier to wrap it in a function that does this, possibly as method on RecordSignature)

// TO DO: unpacking records should support use cases where first (e.g. 'target') arg is optional and the second is not, e.g. `forward 100`, where forward is defined as `forward {turtle: optional turtle, distance: number} -> turtle`; thus, `forward 10, turn 90; forward 20.` would operate on current default turtle, whereas `{name:"Bob", color:red} as turtle; forward 10; turn 90; forward 20.` would operate on a newly created turtle named "Bob" (the code editor being smart enough to recommend semi-colons [pipes] if the user didn't already include them herself).


func evalRecordField<ReturnType: CoercionProtocol>(_ fields: inout [Value], fieldStructure: (name: Name, type: ReturnType), commandScope: Scope) throws -> ReturnType.SwiftType {
//    print("evalRecordField: `\(fieldStructure.name.keyString)` as \(fieldStructure.type), SwiftType=\(ReturnType.SwiftType.self)")
    let fieldValue: Value
    if fields.count > 0 {
        let field = fields[0]
        if let pair = field as? Pair, let name = pair.key as? Name { // field is a literal pair with a literal name
            if name.keyString == fieldStructure.name.keyString { // use field if names match
                fieldValue = pair.value
                fields.removeFirst()
            } else {
                fieldValue = gNullValue // else treat as omitted field
            }
        } else {
            fieldValue = field
            fields.removeFirst()
        }
    } else {
        fieldValue = gNullValue
    }
//    print("\nExpanding `\(fieldStructure.name.keyString)` field to \(fieldStructure.type): `\(fieldValue)`\n")
     do {
        return try fieldValue.evaluate(commandScope, returnType: fieldStructure.type)
    } catch {
        throw MismatchedField(description: "Failed to coerce \(fieldStructure.name) field to \(fieldStructure.type): \(error)") // note: caller will need to catch and rethrow with additional error info (including both records)
    }
}


//**********************************************************************


class Procedure: CustomStringConvertible {
    
    // Note: a procedure is by default stored in the scope in which it was defined and thus lexically bound; to avoid refcycles that scope is passed in upon invocation. This means a Procedure is not itself a closure; thus it is not defined as a Value subclass. Instead, it must be encapsulated by a `PROCNAME as procedure` cast which returns a `Closure` containing both the Procedure and the Scope within which it was found (somewhat like a thunk, except it supports `call` rather than `eval` [the latter would merely return self]). Bear in mind that storing this procvalue in the same scope (or its subscopes) will create a refcycle; will need to decide how best to deal with that (e.g. uncycler could be informed of the procvalue's creation, allowing it to put that scope on its to-check list).
    
    let signature: ProcedureSignature
    var keyString: String { return self.signature.name.keyString }
    // Q. what about declaring side-effects, safety, idempotency?
    
    // TO DO: operator info, if any
    
    init(signature: ProcedureSignature) {
        self.signature = signature
    }
    
    var description: String { return "PROC«\(self.signature)»" }
    
    // note: command and commandScope are intimately related, and might be worth binding the two together as a lightweight (Command,Scope) tuple; procedureScope is the scope within which the Procedure was stored
    
    func call<ReturnType: CoercionProtocol>(_ command: Command, returnType: ReturnType, commandScope: Scope, procedureScope: Scope) throws -> ReturnType.SwiftType { // Q. what is commandScope exactly? (A. it's just args' lexical scope, supplied so sig's typespecs can coerce/eval those args)
        // 1. get command.data (Record)
        // 2. apply that record to signature.data (Record) to create canonical args (dict? array? Record? what about labels? could all depend on primitive vs native Proc implementation), or throw BadCommand error if they can't be lined up (note: Coercions throw coercion error; mismatched fields would need to throw something else, e.g. BadName; these get caught and rethrown as BadCommand, though the real prize would be to allow these errors to be handled - either by `catching` clause or by user herself - without unspooling call stack)
        fatalNotYetImplemented(self, #function)
    }
}


class PrimitiveProcedure: Procedure {
    // TO DO: this should be glue generated (Q. what, if any, functionality does this base class need to implement? also seems a little redundant since only one instance will ever be needed for each primitive proc; could we reasonably avoid the extra code bloat of 100s of custom subclasses by creating one instance per proc and just code-generating a func wrapper that puts a standard API around Swift func and generates all the bridging code within that; a simple `register` call could then instantiate the PrimitiveProc class to hold the signature and the wrapper func; not polluting Swift namespace nearly as much)
    // also, gluegen should take into account presence/absence of procedureScope and/or commandScope param in Swiftfunc (which in turn might provide some useful metadata about func's scope [sic] of effect); bear in mind too that function's params might already be defined and named [if it's preexisting Swift func co-opted for use here], and those names may not match those in signature; plus existing Swift funcs won't throw native entoli errors so some extra wrapping will be needed there (including in cases where func indicates error condition by returning nil with no additional info)
    
    
    enum Env {
        case none
        case commandScope
        case procedureScope
        case procedureBody
        case fullClosure
    }
    
    typealias FunctionWrapper = (_ commandArguments: [Value], _ commandScope: Scope, _ procedureScope: Scope) throws -> Value
    
    let envType: Env
    let function: FunctionWrapper
    
    init(signature: ProcedureSignature, envType: Env, function: @escaping FunctionWrapper) { // TO DO: related flags (e.g. to indicate if it needs its own subscope, or if it should implement `yield`-style generator)
        self.envType = envType
        self.function = function
        super.init(signature: signature)
    }
    
    convenience init(name: String, type: (parameterType: RecordSignature, returnType: Coercion), envType: Env, function: @escaping FunctionWrapper) {
        self.init(signature: ProcedureSignature(name: Name(name), parameterType: type.parameterType,
                                                returnType: type.returnType), envType: envType, function: function)
    }
    
    // convenience constructors for standard math operators
    
    static let leftFieldSignature         = FieldSignature(gLeftOperandName, gScalarCoercion)
    static let rightFieldSignature        = FieldSignature(gRightOperandName, gScalarCoercion)
    static let scalarInfixParameterType   = RecordSignature(leftFieldSignature, rightFieldSignature)
    static let scalarPrefixParameterType  = RecordSignature(leftFieldSignature)
    static let scalarPostfixParameterType = RecordSignature(rightFieldSignature)
    
    convenience init(name: String, scalarArithmeticOperatorFunction: @escaping ScalarArithmeticFunction) {
        let signature = ProcedureSignature(name: Name(name),
            parameterType: PrimitiveProcedure.scalarInfixParameterType, returnType: gScalarCoercion)
        func wrapperFunction(_ arguments: [Value], commandScope: Scope, procedureScope: Scope) throws -> Value {
            var arguments = arguments
            let arg1 = try evalRecordField(&arguments, fieldStructure: (gLeftOperandName, gScalarCoercion), commandScope: commandScope)
            let arg2 = try evalRecordField(&arguments, fieldStructure: (gRightOperandName, gScalarCoercion), commandScope: commandScope)
            if arguments.count > 0 { throw BadArgument(description: "Too many arguments(s): \(arguments)") }
            return try gScalarCoercion.wrap(try scalarArithmeticOperatorFunction(arg1, arg2), env: procedureScope)
        }
        self.init(signature: signature, envType: .none, function: wrapperFunction)
    }
    
    convenience init(name: String, scalarComparisonOperatorFunction: @escaping ScalarComparisonFunction) {
        let signature = ProcedureSignature(name: Name(name),
                        parameterType: PrimitiveProcedure.scalarInfixParameterType, returnType: gBoolCoercion)
        func wrapperFunction(_ arguments: [Value], commandScope: Scope, procedureScope: Scope) throws -> Value {
            var arguments = arguments
            let arg1 = try evalRecordField(&arguments, fieldStructure: (gLeftOperandName, gScalarCoercion), commandScope: commandScope)
            let arg2 = try evalRecordField(&arguments, fieldStructure: (gRightOperandName, gScalarCoercion), commandScope: commandScope)
            if arguments.count > 0 { throw BadArgument(description: "Too many arguments(s): \(arguments)") }
            return try gBoolCoercion.wrap(try scalarComparisonOperatorFunction(arg1, arg2), env: procedureScope)
        }
        self.init(signature: signature, envType: .none, function: wrapperFunction)
    }
    
    // TO DO: how practical to make Proc.call() methods non-generic? (it should be enough for them just to return Value; the entoli runtime doesn't care, and primitive callers can probably live with it)

    override func call<ReturnType: CoercionProtocol>
        (_ command: Command, returnType: ReturnType, commandScope: Scope, procedureScope: Scope) throws -> ReturnType.SwiftType where ReturnType: Coercion, ReturnType.SwiftType: Value {
        let tmpValue = try self.function(command.argument.fields, commandScope, procedureScope)
        return try tmpValue.evaluate(procedureScope, returnType: returnType) // TO DO: problem: how to intersect proc's returnType with with caller's requested returnType? (there is an implicit constraint here in that returnType's CoercionProtocol.SwiftType should always be Value)
    }
    

}


//**********************************************************************


class NativeProcedure: Procedure {
    
    let body: Value
    
    init(signature: ProcedureSignature, body: Value) {
        self.body = body
        super.init(signature: signature)
    }
    
    // TO DO: need to think about `yield` (the latter should be a feature of Closure [subclass?], which needs to capture proc's own body subscope instead of its lexical scope, and also provide some kind of `isEmpty` flag that is set once proc returns instead of yields; note that `return` command can probably implement optional `resumable` arg, avoiding need for a separate, less familiar, 'yield' command)
    
    override func call<ReturnType: CoercionProtocol>(_ command: Command, returnType: ReturnType, commandScope: Scope, procedureScope: Scope) throws -> ReturnType.SwiftType where ReturnType: Coercion, ReturnType.SwiftType: Value {
        let subEnv = procedureScope.makeSubScope()
        var arguments = command.argument.fields // TO DO: if command takes previous result as its first arg and/or has postfixed `do` block as its last arg, these will need to be passed too
        for (i, parameter) in (self.signature.argument as! RecordSignature).fieldTypes.enumerated() {
            print("getting parameter \(i+1): \(parameter)")
            let value = try evalRecordField(&arguments, fieldStructure: (parameter.name, parameter.type.intersect(gAnyValueCoercion, env: commandScope)), commandScope: commandScope) // TO DO: FIX; parameter.type is Coercion, but evalRecordField needs something that conforms to CoercionProtocol (which, conversely, parameter.type can't be cast to because it's generic); for now we cheat it by doing a redundant intersect whose only real , but this is hardly ideal; one option might be to define a 'native coercion' wrapper for primitive coercions that hooks their expand/coerce methods to their wrap methods, but even that will probably complain
            try subEnv.store(parameter.keyString, value: value)
        }
        if arguments.count > 0 { throw BadArgument(description: "Unrecognized extra argument(s): \(arguments)") }
        print("built subscope: \(subEnv)")
        do {
            return try self.body.evaluate(subEnv, returnType: returnType)
        } catch {
            throw EvaluationError(description: "Error in \(command) call to \(self.signature): \(error)")
        }
    }
}


//**********************************************************************


class MultipleProcedure: Procedure {

    // TO DO: implement procedure overloading support: `-` op is both prefix and infix, so requires two procedures with same name and different signatures: {left,right} and {right}; need to think about how this is going to work, as current design only allows one proc per slot (which is probably wise as it keeps implementation simple for common case and can just use a 'multiproc' wrapper, although it also raises questions about how to deal with masking, since a proc in a child scope will normally 'replace' identically named proc in parent scope, whereas we only want to mask a specific signature; also bear in mind that a parent scope may also evolve over time, so it's not just simple case of `store` checking for same name in its parent scopes; nor can we append new ops to parent scope, as they shouldn't be visible outside of the scope in which they were defined, otherwise they're liable to create incredibly nasty and totally unpredictable and uncontrollable crosstalk between libraries, which won't like having their own parents suddenly mutate from under them). 
    //
    // Simplest would be to require all overloads to be defined at once in same scope, using a variation of `do` that takes one or more `signature:expression` pairs (though even that gets fiddly as we've not yet figured out how `catch`, `returning`, etc should be attached to proc def); thus overridding name in a child scope would mask _all_ those definitions, not extend, which is a bit limiting but at least [hopefully] predictable; plus the overriding definition could still choose to delegate to its parent scope if no match is found, which brings back behavioral extension in a [hopefully] reliable and predictable way (TBH, delegating between environment scopes shouldn't really be any different to delegating between instance methods in class inheritance chain, and is something e.g. AppleScript already does out of the box thus avoiding any need for distinct and semi-overlapping but not quite interchangeable 'module' vs 'class/instance' types).
    //
    // FWIW, `to` operator could probably be allowed to check if a slot is already defined in scope, and if it is replace it with an OverloadedProcedure if it isn't already one; it might also be allowed to append to an existing OverloadedProcedure (assuming we don't just make it write-once to keep behavior simple and predictable), avoiding need for a modified form of `to` for defining all variations at once - will need to think about what works best for comprehension and usability, plus there's always the possibility of sealing the multiproc upon first call (although that's not ideal either since there's no guarantee that call won't come at different times, potentially resulting in different sets of signatures being available)
    
    // TO DO: how best to pattern match argument fields? (this could be a giant PITA as different calls have different operand counts, names, and types); really want to merge all the given info into a common table (much as phrase operator definitions are turned into word-matching tree)

}

