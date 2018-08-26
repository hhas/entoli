//
//  constraints/other.swift
//  entoli
//
//


//**********************************************************************
// anything


class AnyValueConstraint: Constraint, SwiftConstraint, NativeConstraint { // by default, allows anything *except* gNullValue // TO DO: what name? also, how best to implement primitive equivalent? (ideally would be a generic that takes task-specific enum type defined by client code, but can't see how that would work; alternative is just to return Any)
    
    typealias SwiftType = Value // TO DO
    
    // TO DO: option to constrain to one or more specified native Constraint types (i.e. implicit union) e.g. `any [text, list, record]`; this'll probably need to be a list, since order is significant (also needs to do two passes: first to check for exact type match, second to try coercing; oh, and first pass should also check for best partial match for lists and records since complex data structures will tend to match structurally rather than on nominal type)
    
    func coerce(_ value: Value, env: Scope) throws -> Value {
        return try value._expandAsAny_(env)
    }
}


let gAnyValueConstraint = AnyValueConstraint() // any value except `nothing`

let gAnythingConstraint = MayBeNothing(type: gAnyValueConstraint) // any value including `nothing`



//**********************************************************************
// boolean constraints

// TO DO: how this is implemented will depend on whether Boolean tests return traditional `true`/`false` names or Icon-style success/failure (if the latter, then a failed test will need to be represented a special symbol similar to `nothing`, while the original value can be returned as-is on success) [another possibility is for `nothing` to be 'false' and everything else to be 'true' - ideally there'd only be one special name, but since `nothing` may be returned by some APIs on success then a different name is probably required to differentiate it; e.g. would `did nothing` be sufficient to describe both a failed `a > b` test and a no-op `if {test,expr}` where test failed]

// TO DO: native BooleanConstraint that returns Value?

// note: using `nothing` as false is unsatisfactory, as it can't distinguish false from omitted (i.e. use default value, which could be either false or true); so it's either use empty vs non-empty values, or use dedicated true/false constants, or use Icon-style value vs `failed`

class BoolConstraint: Constraint, SwiftConstraint {
    
    typealias SwiftType = Bool
    
    override func defaultValue(_ env: Scope) throws -> Value { return Text("FALSE") } // TO DO: fix once boolean representations and behavior are decided
    
    func unpack(_ value: Value, env: Scope) throws -> SwiftType {
        fatalNotYetImplemented(self, #function) // TO DO: ditto
    }
    
    func pack(_ rawValue: SwiftType, env: Scope) throws -> Value {
        return Text(rawValue ? "OK" : "") // TO DO: ditto
    }
}


let gBoolConstraint = BoolConstraint()


