//
//  constraints/commands.swift
//  entoli
//
//




//
class NameConstraint: Constraint, SwiftCast, NativeConstraint {
    
    typealias SwiftType = Name
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try value._expandAsName_(env) // TO DO: not sure about this (it doesn't expand, but rather typechecks value to see if it's a Name and throws constraint error if not; TBH name literals are a huge pain since they're ambiguous with commands, and must sometimes be treated as name literals - e.g. record keys - and other times as arg-less commands, e.g. record values and most other contexts; worse, a list of name values won't roundtrip when formatted as literal then reparsed [unless formatter knows to e.g. wrap them in `as name` casts])
    }
}


class KeyStringConstraint: Constraint, SwiftCast { // TO DO: use `KeyConstraint`? (the implication being that 'Key' = Name as normalized String); if so, change `keyString` to `key` in other APIs (where the 'String' part is already redundant); one caveat is that Pair already uses `key:Value,value:Value`, where `key` is either a Name (record field) or any Value (in a key-value list item) - could rename that to `label:Value,value:Value`, or even neutral `left:Value,right:Value` (note: for obvious reasons we want to be careful about using the same jargon to mean different things in native vs primitive APIs, and 'key' is already used in 'key-value list')
    
    typealias SwiftType = String
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try value._expandAsName_(env).keyString
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        return Name(rawValue)
    }
}


class CommandConstraint: Constraint, SwiftCast, NativeConstraint {
    
    typealias SwiftType = Command
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try value._expandAsCommand_(env)
    }
}




//**********************************************************************
//


class TypeConstraint: Constraint, SwiftCast, NativeConstraint { // coerce value to a Constraint instance
    
    typealias SwiftType = Constraint
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        if let constraint = value as? Constraint { return constraint } // TO DO: this is no good, since a ProxyConstraint needs to be fully evaled to return actual Constraint instance
        
        // TO DO: can/should this call value.toCommand() and evaluate? (not sure about this)
        
        fatalNotYetImplemented(self, #function)
    }
}



class ParameterTypeConstraint: Constraint, SwiftCast, NativeConstraint {
    
    typealias SwiftType = ParameterType // RecordSignature
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try value._expandAsRecord_(env).toRecordSignature()
    }
}




//**********************************************************************
// commonly used Constraints, predefined for convenience

// TO DO: rename `asAnyValue`, `asText`, `asString`, etc? (need to decide naming convention for standard and special constraint classes, and their convenience constants; e.g. `cAnyValue`, `cText`, etc. would still be preferable to `gAnyValueConstraint`, `gTextConstraint`)


let gNameConstraint = NameConstraint()
let gKeyStringConstraint = KeyStringConstraint()

let gCommandConstraint = CommandConstraint()

let gTypeConstraint = TypeConstraint()

let gParameterTypeConstraint = ParameterTypeConstraint()
let gReturnTypeConstraint = gTypeConstraint


