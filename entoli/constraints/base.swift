//
//  constraints/base.swift
//  entoli
//
//


// TO DO: rename convenience constants from `gTYPEConstraint` to `asTYPE`; e.g. `asAnyValue`, `asText`, `asString`, etc? (need to decide naming convention for standard and special constraint classes, and their convenience constants; e.g. `cAnyValue`, `cText`, etc. would still be preferable to `gAnyValueConstraint`, `gTextConstraint`)



// TO DO: implement `description` methods (simplest would be to implement `toCommand()` for all constraints, then call that to obtain the corresponding constructor command and render that) [caveat we need to iron out `as` operator's behavior and/or install standalone constraint procs, which in turn may influence how constraint commands are written; not to mention some constraints, e.g. `editable`, `optional` will have operators as well]



// TO DO: how to describe - and unpack - arguments whose types are related to each other? e.g. in scalar operators, both operands need to be same primitive type (Int OR Double); in numeric operations, `+` and `-` require two numbers with same unit types (i.e. identical or inter-convertible, e.g. `2m + 3m`, `2m + 300cm`) while `*` and `/` require one numeric and one scalar to produce linear result (e.g. `4m / 2 => 2m`) or (if supported) might also accept two numerics (e.g. `2m * 3m => 6sqm`); this suggests current unpacking process - where each record field is matched and evaluated in turn - is only suitable for simple use cases, and a more sophisticated option where all fields are matched first, then grouped according to relationships and then further matched as groups to determine appropriate/optimal constraint[s] to apply to them (e.g. `&` operator needs to coerce both operands to same outer type, e.g. text&text or list&list or record&record); plus such matching needs to be table- and/or matchfunc-driven so that it's fully extensible



// TO DO: would this stuff work better if `evaluate` entry point was on Constraint rather than Value? (e.g. ListConstraint really wants to tell ItemType to coerce to native, regardless of what primitive type it has; also, would passing a `type:Any.Type` arg to evaluate and letting runtime decide what constraint to apply work better?); bear in mind primitive procs need to have everything typechecked at compile-time and also want everything to be hardwired for easy conversion to static Swift code, so table-based lookup is non-optimal there


//**********************************************************************
// Constraint base class and protocols
//
// Defines methods that all constraints must implement, and provides default implementations for some where appropriate.
//
// Important: all constraint subclasses must inherit from Constraint base class, and add SwiftCast once API-complete. (This is a pain, but it keeps Swift's generics system happy when bridging between entoli Values and Swift types [Int, String, Array<...>, etc.], which in turn simplifies and standardizes glue code for primitive procedures.)
//
// In addition, native constraint classes (i.e. those that convert Value <-> Value) should adopt the NativeCoercion protocol, allowing them to be used directly by entoli code.
//
// Primitive constraint classes are used in primitive libraries to bridge entoli Values to and from Swift values. To allow interface introspection, these should override `Constraint.nativeConstraint` to return the corresponding NativeCoercion.


// TO DO: would it make sense to define protocols for both PrimitiveConstraint, NativeConstraint, plus a base protocol, and have only NativeConstraint subclasses inherit from Constraint? (this'd prevent primitive constraints moving through the entoli object system, which might be sensible given that they can't be used directly within entoli to convert values [although they could still be summed and intersected, and converted to NativeConstraints on demand, so the flipside argument is that if it doesn't do any harm then leave them as-is until if/when they're actually used and convert them then, and avoids the need to encapsulate them in a dedicated OpaqueNativeConstraint wrapper if they do need to move through it, e.g. when metaprogramming around primitive procs])


class Constraint: Value {
    
    // TO DO: Constraints should be Hashable and Equatable, allowing values to cache previously coerced representations of themselves, in addition to their tagged type(s)
    
    override func toCommand() throws -> Command { // all concrete subclasses must override this (unlike _expand..._, converts without evaluation)
        // TO DO: [also] implement _expandAsCommand_? (TBH, not sure how constraints should behave as values; suspect they need a Proc [sub]class for constructing them; _expandAsCommand_ would only be useful in metaprogramming for converting constraint values back to the commands that constructed them)
        fatalNotYetImplemented(self, #function) // TO DO: is this appropriate (all constraint classes _should_ be able to provide their corresponding constructor command)? or should it just throw an error if a particular constraint object can't provide a native constructor command? (another possibility is to return a command with an opaque value: that way it can still be used as a command, returning itself when command is evaled; just not converted to literal code)
    }
    
    override func _expandAsAny_(_ env: Scope) throws -> Value {
        return self
    }
    
    var defersExpansion: Bool { return false } // determines if Thunk.evaluate() should force and return its thunked value, or call ReturnType._coerce_(self,...) and let it decide what it wants to do with the Thunk (e.g. DoNotEvaluate will return Thunk unchanged, ThunkConstraint with thunk it again) // IMPORTANT: only special-case NativeConstraints should ever return true (e.g. ThunkConstraint, ExpressionConstraint) [might want to add an assert for that]
    
    func defaultValue(_ env: Scope) throws -> Value {
        // called by DefaultValue constraint when no value is given and it doesn't contain a default value itself; this avoids the need to write `DefaultValue(type: textConstraint, value: Text(""))`,  `DefaultValue(type: intConstraint, value: Text("0"))`, etc. since obvious defaults for these types can be inferred. One downside is that there's no way to check additional constraints, e.g. `DefaultValue(type: IntConstraint(min:10))` will throw a runtime error; OTOH, so will `DefaultValue(type: IntConstraint(min:10), Text("0"))`, so the problem isn't specific to inferred defaults, just easier to miss. (This is also why it can't return SwiftType, since constraint checking is performed by _constraint_;.)
        throw ImplementationError(description: "\(self) constraint does not provide a standard default value, and no other default was specified.") // note: the `default` type command (commonly used to define optional parameters to native procedures) should ensure that a valid default always exists or error if not, so the only time this error should occur is if a primitive procedure's signature has forgotten to supply one (i.e. developer error)
    }

    func intersect<ReturnType>(_ returnType: ReturnType, env: Scope) -> ReturnType where ReturnType: Constraint, ReturnType: SwiftCast { // note: the ReturnType is needed as caller wants to call _coerce_
        print("WARNING: Constraint.intersect not implemented for \(self)")
        return returnType
    }
    
    var _cachedNativeConstraint: NativeConstraint?
}



protocol SwiftCast {
    
    associatedtype SwiftType
    
    // TO DO: are these necessary? base classes already define them
    var description: String { get } // subclasses must return their literal constructor representation
    var defersExpansion: Bool { get }
    //func defaultValue(_ env: Scope) throws -> Value
    
    var nativeConstraint: NativeConstraint { get } // TO DO: rename toValue()->NativeConstraint?
    
    
    func intersect<ReturnType>(_ returnType: ReturnType, env: Scope) -> ReturnType where ReturnType: Constraint, ReturnType: SwiftCast // TO DO: what other set operations (`union`, `contains`, `==`, `is[Strict]SubsetOf`, `is[Strict]SupersetOf`) should be supported? note that `contains()` is needed to check that a Value meets Constraint's constraints without coercing it first (i.e. sum types need to check for an exact match first before trying to find a coerced match), and is further complicated by fact that collections may provide a partial exact match (e.g. a list of numbers partially matches a list of text in that both have the same container type, so should be given preferential treatment when trying coerced matches)
    
    // TO DO: `func toCommand() throws -> Command` should return Constraint value's constructor command, e.g. `TextConstraint.toCommand()` -> `text {...}`
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType // called by Value.evaluate(); other code should avoid calling this directly
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value // (this shouldn't normally require env, but it's supplied just in case); can be used to re-wrap the Swift value returned by Value.evaluate(), e.g. `try someType.wrap(someValue.evaluate(env, returnType: someType), env: env)` // TO DO: this would work better if there was a `typealias NativeType` to improve its return type; having both 'before' and 'after' types present may also prove useful elsewhere
    
    func toNative() -> NativeConstraint
}



extension SwiftCast where SwiftType: Value {
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType { // subclasses must override
        fatalNotYetImplemented(self, #function)
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value { // Value->Value constraints don't need to implement custom wrap() methods
        return rawValue
    }
    
    var nativeConstraint: NativeConstraint { return self as! NativeConstraint }
}

extension SwiftCast {
    
    var nativeConstraint: NativeConstraint {
        if (self as! Constraint)._cachedNativeConstraint == nil { (self as! Constraint)._cachedNativeConstraint = self.toNative() }
        return (self as! Constraint)._cachedNativeConstraint!
    }
    
    func toNative() -> NativeConstraint { fatalError("Non-native Constraint subclasses must override this") } // TO DO: return OpaqueNativeConstraint? this'd allow values to be tagged even if th
}


protocol NativeConstraint {
    // TO DO: what does this need to expose?
    // TO DO: need a method to construct NativeConstraint instance with optional record arg containing additional constraints
    // TO DO: how to declare native signature and mapping to SomeNativeConstraint.init(...)? e.g. `text {may be empty: optional Boolean}`
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType // subclasses must override
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value // Value->Value constraints don't need to implement custom wrap() methods
    func defaultValue(_ env: Scope) throws -> Value
    var defersExpansion: Bool {get} // TO DO: might be more appropriate to provide `specialExpansion(value,env)->(value,returnNow)` which gets called by Value.evaluate() prior to/instead of it doing its own thing, in which case maybe add a SpecialExpansion protocol which DoNotExpand, ThunkConstraint, etc adopt; evaluate methods will then check for that protocol (alternatively, it might be simpler always to call specialExpansion which in most cases will be a no-op)
    var nativeConstraint: NativeConstraint { get }
}

extension NativeConstraint {
    typealias SwiftType = Value
}




