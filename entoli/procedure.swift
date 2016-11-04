//
//  procedure.swift
//  entoli-run
//
//
//

// TO DO: how practical for procs to declare they're idempotent? this'd be particularly useful for ProxyConstraints as it'd allow them to memoize the constructed Constraint on first use, rather than reconstructing it every time it's applied; problem is native procs don't know if they're idempotent as commands within their body are late-bound; also bear in mind that type args/operands can't be evaled until scope is fully populated by top-level exprs being evaled, but `to` commands need to eval param types in order to add themselves to scope (thus their full initialization really wants to be deferred until after the scope is fully populated)


// TO DO: decide on 'procedure' vs 'handler' (personal habit tends to the latter, plus it's shorter to write [and same no. of syllables characters as 'command', which is neater], but ultimately it'll depend on which term is more meaningful to users [and 'procedure' probably has the edge there due to its common meaning])


//**********************************************************************


// note: primitive proc glue should code-generate one unpackArgument() call for each parameter in sig
// TO DO: this func should probably be generalized to work on any record, allowing it to be applied by coercion handlers as well (note: would probably be easier to wrap it in a function that does this, possibly as method on RecordSignature)

// TO DO: unpacking records should support use cases where first (e.g. 'target') arg is optional and the second is not, e.g. `forward 100`, where forward is defined as `forward {moving: optional turtle, by: length} -> turtle`; thus, `forward 10, turn 90, forward 20.` would operate on current default turtle, whereas `{name:"Bob", color:red}; forward 10; turn 90; forward 20.` would operate on a newly created turtle named "Bob" (the code editor being smart enough to recommend semi-colons [pipes] if the user didn't already include them herself).


func evalRecordField<ReturnType: SwiftCast>(in fields: inout [Value], fieldStructure: (name: String, type: ReturnType), commandScope: Scope) throws -> ReturnType.SwiftType {
//    print("evalRecordField: `\(fieldStructure.name.keyString)` as \(fieldStructure.type), SwiftType=\(ReturnType.SwiftType.self)")
    let fieldValue: Value
    if fields.count > 0 {
        let field = fields[0]
        if let pair = field as? Pair, let name = pair.key as? Name { // field is a literal pair with a literal name // IMPORTANT: this'll only work as long as `(...)` group exprs are preserved as-is when they appear in record fields; e.g. `{(foo:1)}` is NOT the same as `{foo:1}` (the former is an unlabeled record field containing a Pair value, whereas the latter is a labeled record field named 'foo' containing a Text value)
            if name.keyString == fieldStructure.name { // use field if names match
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
    print("\nExpanding `\(fieldStructure.name)` field to \(fieldStructure.type): `\(fieldValue)`\n")
    do {
        if fieldStructure.type is DoNotEvaluate { return fieldValue as! ReturnType.SwiftType } // TO DO: HACKY; also, what about ThunkConstraint and other deferreds? (e.g. Thunk needs to defer evaluation too; however, its _coerce_ method still needs to be called now so it can wrap the expr in a Thunk and return that.) The problem here is that the coercion needs to be in charge; right now it's ignored until it's used to coerce the last expr's result. Will need to check what Value.evaluate() methods are currently implemented.
        return try fieldValue.evaluate(commandScope, returnType: fieldStructure.type)
    } catch {
        throw MismatchedField(description: "Failed to coerce \(Name(fieldStructure.name)) field from \(type(of:fieldValue)) to \(fieldStructure.type): \(error)") // note: caller will need to catch and rethrow with additional error info (including both records) // TO DO: show value's constraint tag
    }
}


//**********************************************************************


class Procedure: CustomStringConvertible {
    
    // Note: a procedure is by default stored in the scope in which it was defined and thus lexically bound; to avoid refcycles that scope is passed in upon invocation. This means a Procedure is not itself a closure; thus it is not defined as a Value subclass. Instead, it must be encapsulated by a `PROCNAME as procedure` cast which returns a `Closure` containing both the Procedure and the Scope within which it was found (somewhat like a thunk, except it supports `call` rather than `eval` [the latter would merely return self]). Bear in mind that storing this procvalue in the same scope (or its subscopes) will create a refcycle; will need to decide how best to deal with that (e.g. uncycler could be informed of the procvalue's creation, allowing it to put that scope on its to-check list).
    
    let signature: ProcedureSignature
    var keyString: String { return self.signature.name }
    // Q. what about declaring side-effects, safety, idempotency?
    
    // TO DO: operator info, if any
    
    init(signature: ProcedureSignature) {
        self.signature = signature
    }
    
    var description: String { return "PROC«\(self.signature)»" }
    
    // note: command and commandScope are intimately related, and might be worth binding the two together as a lightweight (Command,Scope) tuple; procedureScope is the scope within which the Procedure was stored
    
    func call<ReturnType: SwiftCast>(_ command: Command, returnType: ReturnType, commandScope: Scope, procedureScope: Scope) throws -> ReturnType.SwiftType { // Q. what is commandScope exactly? (A. it's just args' lexical scope, supplied so sig's typespecs can coerce/eval those args)
        // 1. get command.data (Record)
        // 2. apply that record to signature.data (Record) to create canonical args (dict? array? Record? what about labels? could all depend on primitive vs native Proc implementation), or throw BadCommand error if they can't be lined up (note: Constraints throw coercion error; mismatched fields would need to throw something else, e.g. BadName; these get caught and rethrown as BadCommand, though the real prize would be to allow these errors to be handled - either by `catching` clause or by user herself - without unspooling call stack)
        fatalNotYetImplemented(self, #function)
    }
}


class PrimitiveProcedure: Procedure {
    // TO DO: this should be glue generated (Q. what, if any, functionality does this base class need to implement? also seems a little redundant since only one instance will ever be needed for each primitive proc; could we reasonably avoid the extra code bloat of 100s of custom subclasses by creating one instance per proc and just code-generating a func wrapper that puts a standard API around Swift func and generates all the bridging code within that; a simple `register` call could then instantiate the PrimitiveProc class to hold the signature and the wrapper func; not polluting Swift namespace nearly as much)
    // also, gluegen should take into account presence/absence of procedureScope and/or commandScope param in Swiftfunc (which in turn might provide some useful metadata about func's scope [sic] of effect); bear in mind too that function's params might already be defined and named [if it's preexisting Swift func co-opted for use here], and those names may not match those in signature; plus existing Swift funcs won't throw native entoli errors so some extra wrapping will be needed there (including in cases where func indicates error condition by returning nil with no additional info)
    
    
    enum ProcScope { // specifies which, if any, scopes are passed to a primitive procedure function
        case none
        case commandScope // the invoking command's dynamic scope; e.g. used by `store` to store a value in the scope in which it was declared
        case procedureScope // the procedure's lexical scope; e.g. module
        case procedureBody // if given, create a scope for the primitive proc function to use itself
        case fullClosure
    }
    
    typealias FunctionWrapper = (_ commandArguments: [Value], _ commandScope: Scope, _ procedureScope: Scope) throws -> Value
    
    let procScope: ProcScope
    let function: FunctionWrapper
    
    init(signature: ProcedureSignature, procScope: ProcScope, function: @escaping FunctionWrapper) { // TO DO: related flags (e.g. to indicate if it needs its own subscope, or if it should implement `yield`-style generator); also, Swift requires callback function to capture its scope, which may render the procedureScope arg redundant anyway (bit of a C-ism)
        self.procScope = procScope
        self.function = function
        super.init(signature: signature)
    }
        
    // TO DO: how practical to make Proc.call() methods non-generic? (it should be enough for them just to return Value; the entoli runtime doesn't care, and primitive callers can probably live with it)
    
    // TO DO: need to review this, make sure scopes are used correctly
    // command scope allows proc to lazily evaluate its operands in their original scope; procedureScope allows proc to refer to slots within its own lexical scope
    override func call<ReturnType>(_ command: Command, returnType: ReturnType, commandScope: Scope, procedureScope: Scope) throws -> ReturnType.SwiftType where ReturnType: SwiftCast, ReturnType: Constraint, ReturnType.SwiftType: Value {
        let tmpValue = try self.function(command.argument.fields, commandScope, procedureScope)
        return try tmpValue.evaluate(procedureScope, returnType: returnType) // TO DO: problem: how to intersect proc's returnType with with caller's requested returnType? (there is an implicit constraint here in that returnType's SwiftCast.SwiftType should always be Value)
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
    
    override func call<ReturnType>(_ command: Command, returnType: ReturnType, commandScope: Scope, procedureScope: Scope) throws -> ReturnType.SwiftType where ReturnType: Constraint, ReturnType: SwiftCast, ReturnType.SwiftType: Value {
        let subEnv = procedureScope.makeSubScope()
        var arguments = command.argument.fields // TO DO: if command takes previous result as its first arg and/or has postfixed `do` block as its last arg, these will need to be passed too
        for (i, parameter) in self.signature.input.enumerated() {
            print("getting parameter \(i+1): \(parameter)")
            let value = try evalRecordField(in: &arguments, fieldStructure: (parameter.name, parameter.type.intersect(gAnyValueConstraint, env: commandScope)), commandScope: commandScope) // TO DO: FIX; parameter.type is Constraint, but evalRecordField needs something that conforms to SwiftCast (which, conversely, parameter.type can't be cast to because it's generic); for now we cheat it by doing a redundant intersect whose only real , but this is hardly ideal; one option might be to define a 'native coercion' wrapper for primitive coercions that hooks their expand/coerce methods to their wrap methods, but even that will probably complain
            try subEnv.store(parameter.name, value: value)
        }
        if arguments.count > 0 { throw BadArgument(description: "Unrecognized extra argument(s): \(arguments)") }
        print("built subscope: \(subEnv)")
        do {
            return try self.body.evaluate(subEnv, returnType: returnType)
        } catch {
            throw EvaluationError(description: "The \(self.signature) procedure failed while handling the following command:\n\n\t\(command)\n\n\(error)") // TO DO: use chained error
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


