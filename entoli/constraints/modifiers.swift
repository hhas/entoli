//
//  constraints/modifiers.swift
//  entoli
//
//



//**********************************************************************
// no-op


class DoNotEvaluate: Constraint, SwiftConstraint, NativeConstraint { // no-op; unlike AnyValueConstraint, which expands a value to its own choice of type, this immediately returns value without any evaluation; e.g. for use in primitive procedures that want to do their own thing
    
    typealias SwiftType = Value
    
    var defersExpansion: Bool { return true }
    
    let type: Constraint
    
    init(type: Constraint = gAnyValueConstraint) { // for documentation purposes, constructor should take a Constraint object that will provide user with description of what's expected; this Constraint instance can always be used by primitive func if/when it needs to apply it itself
        self.type = type
    }
    
    func coerce(_ value: Value, env: Scope) throws -> Value {
        return value
    }
}


typealias ExpressionConstraint = DoNotEvaluate // TO DO: need to decide namings


//**********************************************************************
// thunk


class ThunkConstraint: Constraint, SwiftConstraint, NativeConstraint { // aka `lazy`
    
    typealias SwiftType = Thunk
    
    let type: Constraint
    
    init(type: Constraint = gAnyValueConstraint) { // TO DO: just how necessary is it to include type here? e.g. lets us specify e.g. `as lazy (list of text)`
        self.type = type
    }
    
    var defersExpansion: Bool { return true }
    
    override func defaultValue(_ env: Scope) throws -> Value { return try self.type.defaultValue(env) }
    
    func coerce(_ value: Value, env: Scope) throws -> Value {
        return Thunk(value: value, env: env, type: self.type) // TO DO: context?
    }
    
}


//**********************************************************************
// optional values


class MayBeNothing<ReturnType>: Constraint, SwiftConstraint, NativeConstraint
        where ReturnType: Constraint, ReturnType: SwiftConstraint, ReturnType.SwiftType: Value {
    
    typealias SwiftType = Value
    
    let type: ReturnType
    
    init(type: ReturnType) {
        self.type = type
    }
    
    var defersExpansion: Bool { return self.type.defersExpansion }
    
    func coerce(_ value: Value, env: Scope) throws -> Value {
        do {
            return try value.evaluate(env, returnType: self.type)
        } catch ExpansionError.nullValue {
            return gNullValue
        }
    }
}


class MayBeNil<ReturnType>: Constraint, SwiftConstraint where ReturnType: Constraint, ReturnType: SwiftConstraint {
    
    typealias SwiftType = Optional<ReturnType.SwiftType>
    
    let type: ReturnType
    
    init(type: ReturnType) {
        self.type = type
    }
    
    var defersExpansion: Bool { return self.type.defersExpansion }
    
    func unpack(_ value: Value, env: Scope) throws -> SwiftType {
        do {
            return Optional.some(try value.evaluate(env, returnType: self.type))
        } catch ExpansionError.nullValue {
            return Optional.none
        }
    }
    
    func pack(_ rawValue: SwiftType, env: Scope) throws -> Value {
        switch rawValue {
        case .some(let v): return try self.type.pack(v, env: env)
        case .none:               return gNullValue
        }
    }
}


//

// TO DO: this should use NativeConstraint where ReturnType.SwiftType:Value

class DefaultValue<ReturnType>: Constraint, SwiftConstraint where ReturnType: Constraint, ReturnType: SwiftConstraint {
    
    typealias SwiftType = ReturnType.SwiftType
    
    let type: ReturnType
    let value: Value?
    
    init(type: ReturnType, value: Value? = nil) {
        self.type = type
        self.value = value
    }
    
    var defersExpansion: Bool { return self.type.defersExpansion }
    
    func unpack(_ value: Value, env: Scope) throws -> SwiftType {
        var expandedValue: SwiftType
        do {
            expandedValue = try value.evaluate(env, returnType: self.type)
        } catch ExpansionError.nullValue {
            let defaultValue: Value
            if self.value == nil {
                do {
                    defaultValue = try self.type.defaultValue(env)
                } catch {
                    throw ConstraintError(value: value, constraint: self, description: "No value was given, and no default was provided: \(error)")
                }
            } else {
                defaultValue = self.value!
            }
            do {
                expandedValue = try defaultValue.evaluate(env, returnType: self.type)
            } catch {
                throw ConstraintError(value: value, constraint: self, description: "Couldn't use standard default: \(error)")
            }
        }
        return expandedValue
    }
    
    func pack(_ rawValue: SwiftType, env: Scope) throws -> Value {
        return try self.type.pack(rawValue, env: env)
    }
}


// TO DO: ditto
class Precis<ReturnType>: Constraint, SwiftConstraint where ReturnType: Constraint, ReturnType: SwiftConstraint { // provides a custom description of Constraint object for documentation purposes
    
    typealias SwiftType = ReturnType.SwiftType
    
    let type: ReturnType
    let _description: String
    
    var defersExpansion: Bool { return self.type.defersExpansion }
    
    override var description: String { return self._description }
    
    init(type: ReturnType, description: String = "") {
        self.type = type
        self._description = description
    }
    
    func unpack(_ value: Value, env: Scope) throws -> SwiftType {
        return try value.evaluate(env, returnType: self.type)
    }
    
    func pack(_ rawValue: SwiftType, env: Scope) throws -> Value {
        return try self.type.pack(rawValue, env: env)
    }
}


//**********************************************************************
// commonly used Constraints, predefined for convenience

// TO DO: rename `asAnyValue`, `asText`, `asString`, etc? (need to decide naming convention for standard and special constraint classes, and their convenience constants; e.g. `cAnyValue`, `cText`, etc. would still be preferable to `gAnyValueConstraint`, `gTextConstraint`)

let gNoResult = Precis(type: gAnythingConstraint, description: "nothing") // TO DO: need to change this to use `NoResult` as it needs to evaluate normally, discard any result that might be given, and return gNullValue

let gDoNotEvaluate = DoNotEvaluate()



