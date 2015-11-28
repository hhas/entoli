//
//  procedure.swift
//  entoli-run
//
//
//

// note: there's a lot of nested calls right now, which will likely need flattened later to minimize risk of blowing Swift stack



// a procedure is, nominally:
//
// * signature + Swift func (primitive) // Q. what about Env? (e.g. module namespace); on eval, make subenv, bind command args+signature typespecs to it (note: unlike kiwi, command args should eval prior to invocation, using the command's env, with only those typed as `lazy` or `expression` being bound 'as-is')
//
// * signature + GroupExpression + Env (lexical scope)

// Q. can/should Procedure be Value? (indeed, how much need is there for procs to act as closures if GroupExpressionValues are first-class and capture their own lexical scope?)

// Q. oh, and to what extent should Commands capture their lexical scope? (technically they only need their lexical scope for as long as it takes to eval their arguments given the proc's signature, since those arguments will either eval eagerly or thunk themselves); the catch is that a single Command [or Name found in a command eval scope] is equivalent to a GroupExpression in that both carry unevaled bindings. I suspect 'context matters' is what saves us here, as they can always be wrapped in an explicit ThunkValue or ConText in order to make them portable *if and when needed* (i.e. they don't need to be self-thunking or otherwise carry their own context, and wouldn't really benefit from it anyway); and composition grants more flexibility (think also Kernel lang and how it decomposes lambda into subenv-creation+arg-binding and evaluation, plus there's the issue of making it straightforward to emit Swiftcode which gets hard if entoli commands diverge too far from swift funccalls in semantics - i.e. an explicitly thunked expr can [at a minimum] simply translate to an unnamed closure with signature `()throws->Value/Swifttype`).

// note: Swift is not dynamic, thus any call into a Swift-based proc will require a glue. (That glue should be machine-generated, mapping entoli proc signature to Swift func signature and unboxing/boxing accordingly. Value type conversions and constraint checks might also be emitted as Swift code; still need to decide how typespecs that convert Entoli types to Swift types should work.)


//**********************************************************************
// Signature classes describe a procedure's entoli interface

// note: subclassing allows these to be manipulated as ordinary record and command types in metaprogramming; coercions should ensure that standard types get coerced to these where necessary

// TO DO: there's some overlap between Signature and Coercion classes; can/should they be unified? (A. probably not; it's better they remain as Values which can optionally parameterize the corresponding Coercion) (also note that FieldSignature is NOT same as PairCoercion, so those two definitely shouldn't be merged)


class FieldSignature: Pair {
    
    var keyString: String { return self.name.keyString }
    let name: Name
    let type: Coercion
    
    init(_ key: Name, _ value: Coercion) {
        self.name = key
        self.type = value
        super.init(key, value)
    }
    
    override func toFieldSignature() throws -> FieldSignature {
        return self
    }
}


class RecordSignature: Record { // TO DO: use generics rather than subclassing? (entoli should always use coercions to ensure it gets the right Value type, which at worst means unboxing and reboxing the Array if going from RecordSignature to Record (which shouldn't be often, and is still cheaper than going the other way), so shouldn't care about subclassing or lack thereof)
    
    let fieldTypes: [FieldSignature]
    
    init(fields: [FieldSignature]) {
        self.fieldTypes = fields
        super.init(fields)
    }
    
    convenience init(_ fields: FieldSignature...) {
        self.init(fields: fields)
    }
}


class ProcedureSignature: Command { //
    
    let returnType: Coercion // TO DO: should this include 'fails'? would be cleaner, esp if using icon-style error handling, which in turn might allow a degree of continuation since remaining procs could be captured by the error value as it passes through them, along with their scope... will really have to think that one through, plus it'd presumably be limited to native procs)
    
    // note: it would be better if signature was simpler (less initialization overhead); alternatively, use structs/tuples to declare primitive procs, and only create ProcedureSignature instances for native procs and when introspecting primitive procs
    
    init(name: Name, parameterType: RecordSignature, returnType: Coercion) { // TO DO: this shouldn't throw as declarations will be top-level in swift
        self.returnType = returnType
        super.init(name: name, argument: parameterType)
    }
}




//**********************************************************************


// note: primitive proc glue should code-generate one unpackArgument() call for each parameter in sig
// TO DO: this func should probably be generalized to work on any record, allowing it to be applied by coercion handlers as well


// note: to normalize record without evaling, use PassThru as requiredType
func evalRecordField<ReturnType: CoercionProtocol>(inout fields: [Value], fieldStructure: (name: Name, type: ReturnType), commandScope: Scope) throws -> ReturnType.SwiftType {
    print("evalRecordField: `\(fieldStructure.name.keyString)` as \(fieldStructure.type.dynamicType)(), SwiftType=\(ReturnType.SwiftType.self)")
    let fieldValue: Value
    if fields.count > 0 {
        let field = fields[0]
        if let pair = field as? Pair, let name = pair.key as? Name { // pair has literal name
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
     do {
        return try fieldStructure.type._coerce_(fieldValue, env: commandScope)
    } catch {
        throw MismatchedField(description: "Failed to coerce \(Name(fieldStructure.name.keyString)) field to \(fieldStructure.type): \(error)") // note: caller will need to catch and rethrow with additional error info (including both records)
    }
}


//**********************************************************************


class Procedure: CustomStringConvertible {
    
    // Note: a procedure is stored in the Scope to which it is lexically bound; thus to avoid refcycles that scope is passed in upon invocation. This means a Procedure is not itself a closure; thus it is not defined as a Value subclass. Instead, it must be encapsulated by a `PROCNAME as procedure` cast which returns a `Closure` containing both the Procedure and the Scope within which it was found (somewhat like a thunk, except it supports `call` rather than `eval` [the latter would merely return self]). Bear in mind that storing this procvalue in the same scope (or its subscopes) will create a refcycle; will need to decide how best to deal with that (e.g. uncycler could be informed of the procvalue's creation, allowing it to put that scope on its to-check list).
    
    // Note that storage of a Closure might be a little finnicky under current design; suspect Scope is going to need to use an enum to store Procedure/Value/ProcValue
    
    let signature: ProcedureSignature
    var keyString: String { return self.signature.name.keyString }
    // Q. what about declaring side-effects, safety, idempotency?
    
    // TO DO: operator info, if any
    
    init(signature: ProcedureSignature) {
        self.signature = signature
    }
    
    var description: String { return "PROC«\(self.signature)»" }
    
    // note: command and commandScope are intimately related, and might be worth binding the two together as a lightweight (Command,Scope) tuple; procedureScope is the scope within which the Procedure was stored
    
    func call<ReturnType: CoercionProtocol>(command: Command, returnType: ReturnType, commandScope: Scope, procedureScope: Scope) throws -> ReturnType.SwiftType { // Q. what is commandScope exactly? (A. it's just args' lexical scope, supplied so sig's typespecs can coerce/eval those args)
        // 1. get command.data (Record)
        // 2. apply that record to signature.data (Record) to create canonical args (dict? array? Record? what about labels? could all depend on primitive vs native Proc implementation), or throw BadCommand error if they can't be lined up (note: Coercions throw coercion error; mismatched fields would need to throw something else, e.g. BadName; these get caught and rethrown as BadCommand, though the real prize would be to allow these errors to be handled - either by `catching` clause or by user herself - without unspooling call stack)
        
        throw NotImplementedError()
    }
}


class PrimitiveProcedure: Procedure {
    // TO DO: this should be glue generated (Q. what, if any, functionality does this base class need to implement? also seems a little redundant since only one instance will ever be needed for each primitive proc; could we reasonably avoid the extra code bloat of 100s of custom subclasses by creating one instance per proc and just code-generating a func wrapper that puts a standard API around Swift func and generates all the bridging code within that; a simple `register` call could then instantiate the PrimitiveProc class to hold the signature and the wrapper func; not polluting Swift namespace nearly as much)
    // also, gluegen should take into account presence/absence of procedureScope and/or commandScope param in Swiftfunc (which in turn might provide some useful metadata about func's scope [sic] of effect); bear in mind too that function's params might already be defined and named [if it's preexisting Swift func co-opted for use here], and those names may not match those in signature; plus existing Swift funcs won't throw native entoli errors so some extra wrapping will be needed there (including in cases where func indicates error condition by returning nil with no additional info)
    
    typealias FunctionWrapper = (commandArguments: [Value], commandScope: Scope, procedureScope: Scope, returnType: Coercion) throws -> Value
    
    let function: FunctionWrapper
    
    init(signature: ProcedureSignature, function: FunctionWrapper) { // TO DO: related flags (e.g. to indicate if it needs its own subscope, or if it should implement `yield`-style generator)
        self.function = function
        super.init(signature: signature)
    }
    
    convenience init(name: Name, parameterType: RecordSignature, returnType: Coercion, function: FunctionWrapper) {
        self.init(signature: ProcedureSignature(name: name, parameterType: parameterType, returnType: returnType), function: function)
    }
    
    // TO DO: how practical to make Proc.call() methods non-generic? (it should be enough for them just to return Value; the entoli runtime doesn't care, and primitive callers can probably live with it)

    override func call<ReturnType: CoercionProtocol where ReturnType: Coercion, ReturnType.SwiftType: Value>
                        (command: Command, returnType: ReturnType, commandScope: Scope, procedureScope: Scope) throws -> ReturnType.SwiftType {
        return try self.function(commandArguments: command.argument.fields, commandScope: commandScope,
                                 procedureScope: procedureScope, returnType: returnType) as! ReturnType.SwiftType
    }
}


//**********************************************************************


class NativeProcedure: Procedure {
    
    let body: ExpressionGroup
    
    init(signature: ProcedureSignature, body: ExpressionGroup) {
        self.body = body
        super.init(signature: signature)
    }
    
    // TO DO: need to think about `yield` (the latter should be a feature of Closure [subclass?], which needs to capture proc's own body subscope instead of its lexical scope, and also provide some kind of `isEmpty` flag that is set once proc returns instead of yields; note that `return` command can probably implement optional `resumable` arg, avoiding need for a separate, less familiar, 'yield' command)
    
    override func call<ReturnType: CoercionProtocol where ReturnType.SwiftType: Value>(command: Command, returnType: ReturnType,
                                                                                       commandScope: Scope, procedureScope: Scope) throws -> ReturnType.SwiftType {
        let subEnv = procedureScope.makeSubScope()
        var arguments = command.argument.fields // TO DO: if command takes previous result as its first arg and/or has postfixed `do` block as its last arg, these will need to be passed too
        for (i, parameter) in (self.signature.argument as! RecordSignature).fieldTypes.enumerate() {
            print("getting parameter \(i+1): \(parameter)")
            let value = try evalRecordField(&arguments, fieldStructure: (parameter.name, parameter.type.intersect(gAnyCoercion, env: commandScope)), commandScope: commandScope) // TO DO: FIX; parameter.type is Coercion, but evalRecordField needs something that conforms to CoercionProtocol (which, conversely, parameter.type can't be cast to because it's generic); for now we cheat it by doing a redundant intersect whose only real , but this is hardly ideal; one option might be to define a 'native coercion' wrapper for primitive coercions that hooks their expand/coerce methods to their wrap methods, but even that will probably complain
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


