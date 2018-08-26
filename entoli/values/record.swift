//
//  values/record.swift
//  entoli
//
//

import Foundation


//**********************************************************************
// colon pair


class Pair: Value { // TO DO: use generic Pair<KeyT,ValueT> rather than subclassing? (problem is getting various methods to compile, plus `is Pair` won't work if required; may be better to keep this as-is)
    
    let key: Value, value: Value
    
    init(_ key: Value, _ value: Value) {
        self.key = key
        self.value = value
    }
    
    // literal representations
    
    // TO DO: `A:B:C` is right-associative; formatter may want to parenthesize RH pair for clarity
    
    override var description: String { return "\(self.key): \(self.value)" }
    override var debugDescription: String { return "Pair(\(self.key.debugDescription), \(self.value.debugDescription))" }
    
    // conversion
    
    func toStoreCommand() throws -> Command { // used in `do` block and script expression sequences only
        guard let key = self.key as? Name else { throw BadArgument(description: "Pair value does not have a valid name: \(self)") }
        let command = Command("store", self.value, key) // TO DO: include field names? Pair(Name("value"), self.value), Pair(Name("named"), name)? also, include read-only flag once that's implemented, as `store` command will probably use read-write as default
        command.annotations.append("Format.StoreOperator")
        return command
    }
    
    func toFieldSignature() throws -> FieldSignature { // TO DO: this is no good, as value is [typically?] a command that must be evaluated to Constraint (and may itself be parenthesized), thus it needs to be evaled to obtain Constraint instance, which requires a fully prepared runtime env, so can only be done at the time the procedure is defined
        guard let key = self.key as? Name else { throw BadArgument(description: "Pair value does not have a valid name: \(self)") }
        
        
        // TO DO: FIX!!! while name is literal, value is an expr that needs evaluated to Constraint (note: need to check expression[group]s aren't going to cause problems, or do anything they shouldn't, e.g. side effects; this is probably job for TypeConstraint); all we can do here is call value.toCommand() and cross fingers that when evaled it returns a Constraint value
        guard let value = self.value as? Constraint else { throw BadArgument(description: "Pair value does not have a valid type: \(self)") }
        return FieldSignature(key, value)
    }
    
    // expansion
    
    // TO DO: would it be possible/practical to parameterize on returnType? (this'd require all _expand... methods to be generics, but assuming it'd work could likely provide a much tidier API since everything could be done using that)
    override func _expandAsPair_<KeyType, ValueType>(_ env: Scope, keyType: KeyType, valueType: ValueType) throws -> Pair
        where KeyType: Constraint, KeyType: SwiftConstraint, KeyType.SwiftType: Value,
        ValueType: Constraint, ValueType: SwiftConstraint, ValueType.SwiftType: Value {
            return try Pair(self.key.evaluate(env, returnType: keyType), self.value.evaluate(env, returnType: valueType))
    }
    
    override func _expandAsAny_(_ env: Scope) throws -> Value {
        return try self._expandAsPair_(env, keyType: gAnyValueConstraint, valueType: gAnyValueConstraint) // TO DO: what if key is Name? pretty sure it needs left as-is, otherwise applying two expands to a pair will change its key; FIX
    }
    
    override func evaluate<ReturnType>(_ env: Scope, returnType: ReturnType) throws -> ReturnType.SwiftType
        where ReturnType: Constraint, ReturnType: SwiftConstraint {
            // TO DO: how should this work? (or is it coercion's job to state whether key should be treated as a literal name, or a command, or whatever?)
            fatalNotYetImplemented(self, #function)
    }
    
}


//**********************************************************************
//


// TO DO: would it simplify things if fields were [(Name?,Value)], or separate keys and values lists were used? not sure storing pairs here helps (the alternative would be for fields to be [NameValuePair], but that's kinda pointless)

// TO DO: what about duplicate names? Use AS semantics, where last replaces first, or reject as malformed (excepting gNullName)

class Record: Value { // roughly analogous to struct, though with different share-vs-copy semantics; note: records may be concatenated, but existing items may only have their values modified; their names are fixed and cannot be added, changed, or removed (the only way to do that is by going meta - which in turn raises question of when/where/how to do that, as a 'compiled' distributable script should generally be sealed against that sort of thing as self-modifying code, while powerful, carries risks too)
    
    // TO DO: one thing about storing existing pairs: mutability requires replacing the pair, not mutating its value in-place; Pair values probably should be permanently immutable -- also consider pair semantics when added to kv-list, which uses Dictionary
    
    let fields: [Value]
    
    init(_ fields: [Value]) { // TO DO: dual list+dict storage for faster index+name lookups // TO DO: enforce name:value pairs/values (simplest probably to map all unnamed fields to pairs with empty names)
        self.fields = fields
    }
    
    convenience init(_ fields: Value...) { // TO DO: dual list+dict storage for faster index+name lookups // TO DO: enforce name:value pairs/values (simplest probably to map all unnamed fields to pairs with empty names)
        self.init(fields)
    }
    
    // literal representations
    
    override var description: String { return "{" + fields.map{$0.description}.joined(separator: ", ") + "}" }
    override var debugDescription: String { return "Record(" + fields.map{$0.debugDescription}.joined(separator: ", ") + ")" }
    
    var count: Int { return fields.count }
    
    func appended(_ field: Value) -> Record {
        return Record(self.fields + [field])
    }
    func appended(_ fields: [Value]) -> Record {
        return Record(self.fields + fields)
    }
    func concatenated(_ record: Record) -> Record {
        return Record(self.fields + record.fields)
    }
    
    
    // special conversion, given a record, convert it to a RecordSignature (assuming all its fields contain Name:Value pairs where the value evals to a Constraint) -- Q. if field contains name only, should that be rejected as ambiguous (since it might be field name or constraint name) or treated as NAME:anything, OR treated as '':TYPE?
    
    func toRecordSignature() throws -> RecordSignature { // result is a record whose fields are `Name:Constraint` pairs // TO DO: this is no good, needs env
        var fields = [FieldSignature]()
        for (i, field) in self.fields.enumerated() {
            if let pair = field as? Pair { // must be literal pair, so no eval
                let fieldType = try pair.toFieldSignature() // TO DO: implement toFieldSignature on Name, returning name:AnyValueConstraint?
                // TO DO: icky coercions; generic Pair would avoid these
                fields.append(fieldType)
            } else if let name = field as? Name { // or literal name, so again no eval
                fields.append(FieldSignature(name, gAnyValueConstraint))
            } else {
                throw BadArgument(description: "Field \(i+1) of record does not have a valid name: \(field)")
            }
        }
        return RecordSignature(fields: fields)
    }
    
    // evaluation
    override func _expandAsAny_(_ env: Scope) throws -> Value {
        return try self._expandAsRecord_(env)
    }
    
    override func _expandAsArray_<ItemType>(_ env: Scope, itemType: ItemType) throws -> [ItemType.SwiftType]
        where ItemType: Constraint, ItemType: SwiftConstraint, ItemType.SwiftType: Value { // TO DO: as with List._expandAsRecord_(), this may be a bit janky
            return try self.fields.map{try $0.evaluate(env, returnType: itemType)}
    }
    
    override func _expandAsRecord_(_ env: Scope) throws -> Record { // TO DO: define RecordConstraintProtocol for use here? // TO DO: this method is wrong: need to pass field types
        // note: when called in `to NAME {...} ...` operator's argument record, returnType: is ParameterTypeConstraint
        return Record(try self.fields.map{try $0.evaluate(env, returnType: gAnyValueConstraint)}) // TO DO: HACK; returnType should be some sort of PairConstraint; with the exact type specified by returnType
    }
    
    override func _expandAsConstraint_(_ env: Scope) throws -> Constraint { // note: returnType is ignored as it should always be gTypeConstraint
        fatalNotYetImplemented(self, #function) // TO DO: `VALUE as {name:type,...}` should coerce VALUE to record of specified form; basically allows much more readable shorthand for `VALUE as record {structure: {NAME:TYPE,...}}`
    }
}


let gEmptyRecord = Record()

