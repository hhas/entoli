//
//  signatures.swift
//  entoli
//
//
// classes that describe a record's structure or procedure's native interface
//


typealias ParameterType = RecordSignature
typealias ReturnType = Coercion


//**********************************************************************
// 


class ProxyCoercion: Coercion, SwiftCast { // wrapper for a coercion constructor Command that has yet to be evaluated; allows a signature to be constructed without an environment being available to construct the specified Coercion instance at the time; subsequently using or evaluating the ProxyCoercion wrapper will evaluate the constructor command to give the actual Coercion to be used
    
    typealias SwiftType = Value
    
    let coercionConstructor: Command
    
    init(command: Command) {
        self.coercionConstructor = command
    }
    
    private func force(_ env: Scope) throws -> Coercion { // TO DO: is there any remotely safe way to memoize?
        // TO DO: this evaluation really needs guaranteed to be safe, idempotent, and without side-effects; could coercion constructor procs [also] be defined in their own restricted scope, independent of standard script scopes for use by `as` operator, `to` proc, etc. If that namespace's contents can be guaranteed then early binding should be possible (main caveat would be where a type command is parameterized with the result of another command, e.g. `number{min:n1,max:n2}`, as that would need to be evaled on each use, but as long as constructor proc declares its param types correctly then type args and constraint args should be easy enough to distinguish)
        return try self.coercionConstructor.evaluate(env, returnType: gTypeCoercion)
    }
    
    // when used as a Value, evaluating returns the Coercion object specified by the command
    
    override func _expandAsCoercion_(_ env: Scope, returnType: Coercion) throws -> Coercion { return try self.force(env) }
    
    override func _expandAsAny_(_ env: Scope, returnType: Coercion) throws -> Value { return try self.force(env) }

    // when used as a Coercion, the command is evaluated to obtain the Coercion object which is then used as its result
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType { // TO DO: problematic as force()'s result isn't SwiftCast
        fatalNotYetImplemented(self, #function)
        //return try value.evaluate(env, returnType: self.force(env))
    }

}


//**********************************************************************


// note: subclassing allows these to be manipulated as ordinary record and command types in metaprogramming; coercions should ensure that standard types get coerced to these where necessary


class FieldSignature: Pair {
    
    
    // TO DO: what about encapsulating unevaled Command in a Coercion wrapper class? when expanded, that will return itself; or else it'll just use the env passed when _coercion_ is called
    
    var keyString: String { return self.name.keyString }
    let name: Name
    let type: Coercion
    
    init(_ key: Name, _ value: Coercion) { // problem: really needs to be SwiftCast, with SwiftType: Value; however, we can't make class generic as arrays of field sigs are always mixed
        self.name = key
        self.type = value
        super.init(key, value) // TO DO: kludgy
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
    
    override func toRecordSignature() throws -> RecordSignature {
        return self
    }
}


class ProcedureSignature: Command { //
    
    let returnType: ReturnType // TO DO: should this include 'fails'? would be cleaner, esp if using icon-style error handling, which in turn might allow a degree of continuation since remaining procs could be captured by the error value as it passes through them, along with their scope... will really have to think that one through, plus it'd presumably be limited to native procs)
    
    // note: it would be better if signature was simpler (less initialization overhead); alternatively, use structs/tuples to declare primitive procs, and only create ProcedureSignature instances for native procs and when introspecting primitive procs
    
    init(name: Name, parameterType: ParameterType, returnType: ReturnType) { // TO DO: this shouldn't throw as declarations will be top-level in swift
        self.returnType = returnType
        super.init(name: name, argument: parameterType)
    }
}


//**********************************************************************


let gEmptyRecordSignature = RecordSignature()


