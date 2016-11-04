//
//  values/other.swift
//  entoli
//
//

import Foundation


//**********************************************************************
// nothing


// note: suspect special names will only work as Name subclasses (or as dedicated subclass[es] of Value) if they're also defined as operators

class NullValue: Value { // TO DO: need to think about how 'constant' names work; need to be defined as [.StoredValue] command in appropriate scope that returns Name; also need to decide if any of these 'constants' are important enough to define as atoms (probably not, as that'd create inconsistency between constant names that are defined as built-ins vs those defined by libraries, e.g. scriptable apps)
    
    // literal representations
    
    let keyString = "nothing" // TO DO: what should native name be? `nothing`? `missing value`? what else...`did nothing`? `test failed`, `evaluation error` [`command failed`?]; note that these 'special' built-in names *must* be reserved in Scope so that users can't accidentally override them (though be aware that named pairs in records still allow the same names to be used as [literal] names on LHS, so will need to give some thought to that [probably correct behavior, since parameter records should permit any keys, even when the same words would have special meaning elsewhere]; though given Swift's stubborn refusal to allow initializers to return anything except specified type there could still be holes where a 'special' name ends up as an ordinary Name instance, so we're going to have to think about suitability/safety of using anything except Name type to represent them, otherwise risk is that primitive code tests for `...is NullValue` rather than `keystring ==...`, which it will want to do for efficiency; suppose if 'special' names are fixed then an enum could be assigned internally within Name class to indicate if name is special or non-special, in which case equality tests should rely on that; this should also not affect record keys; expand methods below would need to check enum to decide what, if any, coercions are allowed, which again is less convenient; the alternative would be for most Name constructors to respect `reservedNames` set and refuse to construct 'special' names by default); fwiw, other possibility might be to make all Name constructors private, forcing client code to use classfuncs that are guaranteed to return the correct [sub]class for any given name string
    
    override var description: String { return self.keyString }
    override var debugDescription: String { return "gNullValue" }
    
    // ugh; given that `nothing` serves a special role (default argument handling), should it be a unique class and defined as operator? (main concern is that it must never be masked by a command; probably need a set of "forbidden names" in Scope.store())
    
    override func _expandAsAny_(_ env: Scope) throws -> Value { throw ExpansionError.nullValue }
    override func _expandAsText_(_ env: Scope) throws -> Text { throw ExpansionError.nullValue }
    override func _expandAsName_(_ env: Scope) throws -> Name { throw ExpansionError.nullValue }
    
    // TO DO: simplify sig by grouping key and value types into tuple
    override func _expandAsPair_<KeyType, ValueType>(_ env: Scope, keyType: KeyType, valueType: ValueType) throws -> Pair
        where KeyType: Constraint, KeyType: SwiftCast, KeyType.SwiftType: Value,
        ValueType: Constraint, ValueType: SwiftCast, ValueType.SwiftType: Value {
            throw ExpansionError.nullValue
    }
    override func _expandAsArray_<ItemType>(_ env: Scope, itemType: ItemType) throws -> [ItemType.SwiftType]
        where ItemType: Constraint, ItemType: SwiftCast {
            throw ExpansionError.nullValue
    }
    override func _expandAsRecord_(_ env: Scope) throws -> Record { throw ExpansionError.nullValue }
    override func _expandAsCommand_(_ env: Scope) throws -> Command { throw ExpansionError.nullValue }
}

let gNullValue = NullValue() // note: the main reason for this being a 'special' value rather than just, say, an empty record is so that it will throw NullValueConstraintErrors within Constraints which ConstraintModifiers, e.g. `DefaultValue` can easily intercept and replace with default value (c.f. kiwi); however, see notes about about potential problems with reliably distinguishing special from non-special names // TO DO: rename `NoValue`? `Nothing`?


//**********************************************************************
// thunk


class Thunk: Value { // TO DO: memoize? (kinda depends on difference between `as lazy value` and `as expression`; if there's no memoization then there's no difference, and a new result is generated each time expand is called - which causes fun e.g. when value is `random number` command; otoh if there is memoization then the result needs to be generated and stored in thunk first time expand is called - which is also fun since side-effects mean that value may be different depending on timing of that first call; in addition, if there's memoization then a separate coercion may be needed to capture as expression [although arguably that's just a degenerate form of `as procedure`])
    
    let value: Value
    let env: Scope
    let type: Constraint // TO DO: kludgy, since we can't currently union thunked and requested Constraints, so have to apply them in sequence
    
    init(value: Value, env: Scope, type: Constraint = gAnyValueConstraint) {
        self.value = value
        self.env = env
        self.type = type
    }
    
    override var description: String {
        return "Thunk(\(self.value), \(self.type))" // TO DO: this needs to escape any `"` chars within self.string by doubling them
    }
    
    override func evaluate<ReturnType>(_ env: Scope, returnType: ReturnType) throws -> ReturnType.SwiftType
        where ReturnType: Constraint, ReturnType: SwiftCast {
            if returnType.defersExpansion {
                return try returnType._coerce_(self, env: self.env)
            } else {
                return try self.type.intersect(returnType, env: self.env)._coerce_(self.value, env: env)
            }
    }
}


