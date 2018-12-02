//
//  values/signatures.swift
//  entoli
//
//
// classes that describe a record's structure or procedure's native interface
//


typealias ParameterType = RecordSignature
typealias ReturnType = Constraint


//**********************************************************************
// 


class ProxyConstraint: BridgingConstraint { // wrapper for a coercion constructor Command that has yet to be evaluated; allows a signature to be constructed without an environment being available to construct the specified Constraint instance at the time; subsequently using or evaluating the ProxyConstraint wrapper will evaluate the constructor command to give the actual Constraint to be used
    
    typealias SwiftType = Value
    
    let coercionConstructor: Command
    
    init(command: Command) {
        self.coercionConstructor = command
    }
    
    private func force(_ env: Scope) throws -> Constraint { // TO DO: is there any remotely safe way to memoize?
        // TO DO: this evaluation really needs guaranteed to be safe, idempotent, and without side-effects; could coercion constructor procs [also] be defined in their own restricted scope, independent of standard script scopes for use by `as` operator, `to` proc, etc. If that namespace's contents can be guaranteed then early binding should be possible (main caveat would be where a type command is parameterized with the result of another command, e.g. `number{min:n1,max:n2}`, as that would need to be evaled on each use, but as long as constructor proc declares its param types correctly then type args and constraint args should be easy enough to distinguish)
        return try self.coercionConstructor.evaluate(env, returnType: gTypeConstraint)
    }
    
    // when used as a Value, evaluating returns the Constraint object specified by the command
    
    override func _expandAsConstraint_(_ env: Scope) throws -> Constraint { return try self.force(env) }
    
    override func _expandAsAny_(_ env: Scope) throws -> Value { return try self.force(env) }

    // when used as a Constraint, the command is evaluated to obtain the Constraint object which is then used as its result
    
    func coerce(_ value: Value, env: Scope) throws -> SwiftType { // TO DO: problematic as force()'s result isn't SwiftConstraint
        fatalNotYetImplemented(self, #function)
        //return try value.evaluate(env, returnType: self.force(env))
    }

}


//**********************************************************************


// note: subclassing allows these to be manipulated as ordinary record and command types in metaprogramming; coercions should ensure that standard types get coerced to these where necessary


class FieldSignature: Pair {
    
    
    // TO DO: what about encapsulating unevaled Command in a Constraint wrapper class? when expanded, that will return itself; or else it'll just use the env passed when _coercion_ is called
    
    var keyString: String { return self.name.keyString }
    let name: Name
    let type: Constraint
    
    init(_ key: Name, _ value: Constraint) { // problem: really needs to be SwiftConstraint, with SwiftType: Value; however, we can't make class generic as arrays of field sigs are always mixed; TO DO: use NativeConstraint instead? (this will require all coercions to implement `nativeConstraint` var)
        self.name = key
        self.type = value
        super.init(key, value) // TO DO: kludgy
    }
    
    convenience init(_ key: String, _ value: Constraint) {
        self.init(Name(key), value)
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


typealias ProcedureInput = [(name: String, type: Constraint)]
typealias ProcedureOutput = Constraint

// TO DO: redesign Signature classes; they may still be Values, but should store only Swift data and wrap that as Values only when introspected from entoli; the rest of the time they're only used by NativeProcedure to coerce and check input and output values, and it'll be perfectly happy with [Arrays of] (keyString,coercion) tuples

class ProcedureSignature: Value { // TO DO: is subclassing Command appropriate? (nope, since signatures shouldn't be evaluated as commands); Q. should `returning` be an operator that returns a ProcedureSignature value when given the appropriate args? (see also TODO discussion on `to` operator)
    
    let name: String
    let input: ProcedureInput
    let output: ProcedureOutput // TO DO: should this include 'fails'? would be cleaner, esp if using icon-style error handling, which in turn might allow a degree of continuation since remaining procs could be captured by the error value as it passes through them, along with their scope... will really have to think that one through, plus it'd presumably be limited to native procs)
    
    // note: it would be better if signature was simpler (less initialization overhead); alternatively, use structs/tuples to declare primitive procs, and only create ProcedureSignature instances for native procs and when introspecting primitive procs

    // TO DO: sort out the following inits, as they're currently kludges
    
    init(name: String, input: ProcedureInput, output: ProcedureOutput) {
        // TO DO: KLUDGE; this isn't ideal: we want original case for documentation, but normalized for runtime lookup; also, primitive modules can ensure lowercased names at code-generation-time, so will want to skip the lowercased() calls at load-time; one option would be to have primitive and native procsig variants (with a common Swift protocol to ensure interchangeability if/when using them directly in Swift code); meantime, the native variant will receive everything as Values, so can get the lowercased strings from Name.keyString
        self.name = name.lowercased()
        self.input = input.map({($0.lowercased(), $1)})
        self.output = output
    }
    
    convenience init(name: Name, input: RecordSignature, output: Constraint) { // used by `defineProcedure` when defining a native signature; this may well change in future if a native `returning` operator for creating ProcedureSignatures directly is added (which would probably make sense, although it'll still need an operator-less representation, e.g. `{NAME, INPUT, OUTPUT}` which can coerce to signature or, with addition of 4th field, `EXPR`, describe a full procedure as passed to the `to` command); BTW, don't forget that records of form `{NAME:TYPE,...}` should be able to coerce to RecordSignatures, which in turn will also be used when representing/matching complex data structures
        self.init(name: name.string, input: input.fieldTypes.map({($0.name.string, $0.type)}), output: output)
    }
    
    // TO DO: define entoli properties that return name, input, and output info as Values
}


//**********************************************************************


let gEmptyRecordSignature = RecordSignature()


