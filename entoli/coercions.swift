//
//  coercions.swift
//  entoli-run
//
//
//


// hideous call chain:

// given a value and required result type -> EVALUATE -> result of type Coercion.SwiftType
//
//    Scope.evaluate() -> Coercion._coerce_() -> Value._expandAs...()_ // for simple values
//
//    Scope.evaluate() -> ThunkCoercion._coerce_() -> Thunk(); Thunk.evaluate() -> Coercion.intersect() -> Coercion._coerce_() -> Value._expandAs...()_ // for thunks
//



// TO DO: Coercion instances should be able to perform set operations (isSubset, union, etc), eliminating double application



// note: `NAME as name` coercion will treat NAME as name literal if it is a Name value; to make it eval NAME as arg-less command, wrap it in parens, e.g. `(NAME) as name` (fortunately, `as` is fully auto-delimiting so never requires parens for disambiguation); in NameCoercion.resolve, make sure to typecheck passed value and return directly if it's a Name (note that names are one of those areas where conceptual clarity gets rather murky, as how they evaluate is heavily context-dependent: in some places, e.g. record fields, they're treated as symbolic name literals; in others as arg-less commands to be performed; furthermore, names can be added to scopes as procedures that return themselves; unfortunately, the alternative is to use different syntax for names vs commands, e.g. prefixing `@` to names, but that'll look nastly and will still cause confusion amongst novices as to when to include prefix and when not)



// note: evaling a type command gives a Constraint object (i.e. something that describes the structural representation + any value constraints that a unit of data is required to adhere to, whether by good fortune of already meeting those requirements or by being converted to that structure and constraint-checked, throwing an error if either fails)

// TO DO: comparable() method; as in kiwi (Q. should this return another coercion handler, or mapper, or comparable value?), used by comparision, sort, etc. to get normalized values from standard and comparison-only typespecs (e.g. `number`, `case-insensitive text`) that can be directly compared (which will no doubt be a bundle of fun given Swift's type system)

// (basically need to think about class structure; should primitives subclass natives, or vice-versa, or use protocols to cut across? also, want to avoid kiwi problem where most primitive typespecs don't know how to emit their own native command representation, which is something every entoli coercion handler should know how to do, not least because those representations should often be sufficiently 'English-like' to appear directly in user documentation)

// here's where things get sticky, as while ReturnType is a Value when working with native values only, it can be many different types when coercing down to primitive [Swift] types for use in primitive procs; one option would be to use dict of form [(eType,esType):converter,...], though that only works where ReturnType is not a generic (Array,Set,Dictionary), plus we also don't want to have to coerce Value->Value->sType as the extra boxing+unboxing is a waste of time - only thing would be if coercion handlers used the inefficient form for interpretation but emitted dedicated conversion code directly for compilation, in which case there might be some benefit to being able to reason away parts of the process

// TO DO: coercion handler value and/or coercion command; i.e. how best to construct and encapsulate Coercion objects as values? unlike kiwi, they do not apply directly to input values, but are only applied via `as` operator, args to procs [e.g. sort, comparison], Proc.call(), etc; therefore they can appear as opaque values that implement primitive API; whether or not they should subclass Value or just be wrapped by one is to be decided


// am inclined to use primitive procs as coercion constructors; the resulting value can then be bound to a name ('user-defined coercion', avoiding need for explicit kiwi-style `define type` command)



// note: could use ObjectIdentifier(VALUE.self) to build hashable keys when coercing from/to Value types, allowing all such coercions to be registered in a lookup table (caveat global tables are no good; like JSCore, we want to maximise isolation between interpreter instances)

// TO DO: what about Arrays, etc? // TO DO: this buggers things up, making it a generic-only constraint
// TO DO: need FromType too?


// TO DO: there's something valuely smelly about using generics here; shouldn't all type specs provide two overloaded coerce methods, one that returns Swift type and one that returns entoli type?

// TO DO: how to implement comparableValue() method that returns comparable values for use as sort and comparison keys (e.g. TextCoercion would return lowercaseString as entoli does case-insensitive text comparisons by default; CaseSensitiveTextCoercion would return string as-is)


//**********************************************************************
// Coercion protocol
//
// Defines methods that all coercions must implement, and provides default implementations for some where appropriate.
// Important: all coercion subclasses must inherit from Coercion base class, and add CoercionProtocol once API-complete.
// (This is a pain, but it keeps Swift's generics system happy when bridging between entoli Values and Swift types
// [Int, String, Array<...>, etc.], which in turn simplifies and standardizes glue code for primitive procedures.)


protocol CoercionProtocol {
    
    typealias SwiftType
    // TO DO:  {...}; note: this should always return a Value, which is then coerced (except... are there cases where no sensible default exists? e.g. what would a default Pair look like? may want to make this a proc that throws, or else an Optional, or return gNullValue -- the last of these might work best, implemented in CoercionProtocol extension, allowing concrete implementations to override if they wish)
        
    func defaultValue(env: Scope) throws ->  Value // subclasses should override for coercions where a logical default exists (e.g. "", 0, [], {})
    
    func _coerce_(value: Value, env: Scope) throws -> SwiftType // called by Value.evaluate(); other code should avoid calling this directly
    
    func wrap(value: SwiftType, env: Scope) throws -> Value // (this shouldn't normally require env, but it's supplied just in case); can be used to re-wrap the Swift value returned by Value.evaluate(), e.g. `try someType.wrap(someValue.evaluate(env, returnType: someType), env: env)` // TO DO: this would work better if there was a `typealias NativeType` to improve its return type; having both 'before' and 'after' types present may also prove useful elsewhere
    
    func intersect<ReturnType: CoercionProtocol where ReturnType: Coercion>(returnType: ReturnType, env: Scope) -> ReturnType // TO DO: what other set operations (`union`, `contains`, `==`, `is[Strict]SubsetOf`, `is[Strict]SupersetOf`) should be supported? note that `contains()` is needed to check that a Value meets Coercion's constraints without coercing it first (i.e. sum types need to check for an exact match first before trying to find a coerced match), and is further complicated by fact that collections may provide a partial exact match (e.g. a list of numbers partially matches a list of text in that both have the same container type, so should be given preferential treatment when trying coerced matches)
    
    //func toNative(env: Scope) -> Coercion
}

extension CoercionProtocol {
    /*
    typealias T = Self
    
    func toNative(env: Scope) -> NativeCoercion {
        return SwiftType.self is Value.Type ? self as! Coercion : (NativeCoercionWrapper<T>(self) as Coercion)
    }*/
    
    func _coerceToValue_(value: Value, env: Scope) throws -> Value {
        return try self.wrap(self._coerce_(value, env: env), env: env)
    }
}

extension CoercionProtocol where SwiftType: Value { // Value->Value coercions don't need to implement custom wrap() methods
    
    /*func toNative(env: Scope) -> Coercion {
        return self as! Coercion
    }*/
    
    func wrap(value: SwiftType, env: Scope) throws -> Value {
        return value
    }
}


//**********************************************************************
// base class for all coercions (all subclasses must inherit from this)


class Coercion: Value { // TO DO: naming? (TypeConstraint? in entoli, it'll probably need to be `type` or `type command` [`type name` would only work if all constraints were stripped, which violates non-lossy rule and makes some type constraints useless, e.g. multi-choice])
    
    override var description: String {
        return "\(self.dynamicType)" // TO DO: subclasses should return string representing the Swift literal for [re]constructing that particular Coercion instance; this will be used in primitive procedure glue generator to produce working glue code, so must be 100% valid Swift code (this includes formatting Swift primitives correctly)
    }
    
    var defersExpansion: Bool { return false } // determines if Thunk.evaluate() should force and return its thunked value, or call ReturnType._coerce_(self,...) and let it decide what it wants to do with the Thunk (e.g. NullCoercion will return Thunk unchanged, ThunkCoercion with thunk it again)
    
    func defaultValue(env: Scope) throws ->  Value {
        throw ImplementationError(description: "\(self) coercion does not provide a standard default value, and no other default was specified.") // note: the `default` type command (commonly used to define optional parameters to native procedures) should ensure that a valid default always exists or error if not, so the only time this error should occur is if a primitive procedure's signature has forgotten to supply one (i.e. developer error)
    }
    
    func intersect<ReturnType: CoercionProtocol where ReturnType: Coercion>(returnType: ReturnType, env: Scope) -> ReturnType { // TO DO: merge self and `returnType` Coercions into a new Coercion that combines the constraints of both (i.e. describing the [sub]set of all possible values acceptable to both) and returns the resulting data as the type specified by `returnType`
        return returnType // CAUTION: this currently won't return correct type, so native procs' return type will be ignored
    }
    
    
    // TO DO: how best to implement method for returning corresponding command? (e.g. define ProcedureSignature, then use that to build command [semi-]automatically, given args list?)
    
    override func _expandAsCommand_(env: Scope, returnType: Coercion) throws -> Command {
        throw NotImplementedError()
    }
}



//**********************************************************************
//


class AnyCoercion: Coercion, CoercionProtocol {
    
    // TO DO: option to constrain to one or more specified native Coercion types (i.e. implicit union) e.g. `any [text, list, record]`; this'll probably need to be a list, since order is significant (also needs to do two passes: first to check for exact type match, second to try coercing; oh, and first pass should also check for best partial match for lists and records, since)
    
    func _coerce_(value: Value, env: Scope) throws -> Value {
        return value
    }
}

let gAnyCoercion = AnyCoercion()


//


class NullCoercion: Coercion, CoercionProtocol { // no-op; unlike AnyCoercion, which expands a value to its own choice of type, this immediately returns value without any evaluation; e.g. for use in primitive procedures that want to do their own thing
    
    override var defersExpansion: Bool { return true }
    
    let displayType: Coercion
    
    init(displayType: Coercion = gAnyCoercion) { // for documentation purposes, constructor should take a Coercion object that will provide user with description of what's expected; this Coercion instance can always be used by primitive func if/when it needs to apply it itself
        self.displayType = displayType
    }
    
    func _coerce_(value: Value, env: Scope) throws -> Value {
        return value
    }
}


class ThunkCoercion: Coercion, CoercionProtocol { // aka `lazy`
    
    override var defersExpansion: Bool { return true }
    
    let thunkedType: Coercion
    
    init(returnType: Coercion = gAnyCoercion) { // TO DO: just how necessary is it to include type here? e.g. lets us specify e.g. `as lazy (list of text)`
        self.thunkedType = returnType
    }
    
    override func defaultValue(env: Scope) throws -> Value { return try self.thunkedType.defaultValue(env) }
    
    func _coerce_(value: Value, env: Scope) throws -> Thunk {
        return Thunk(value: value, env: env, type: self.thunkedType) // TO DO: context?
    }
}


//**********************************************************************
//


class TuplePairCoercion<KeyCoercion: CoercionProtocol, ValueCoercion: CoercionProtocol where KeyCoercion: Coercion, ValueCoercion: Coercion>: Coercion, CoercionProtocol {
    
    typealias SwiftType = (KeyCoercion.SwiftType, ValueCoercion.SwiftType)
    
    let left: KeyCoercion
    let right: ValueCoercion
    
    init(left: KeyCoercion, right: ValueCoercion) {
        self.left = left
        self.right = right
    }
    
    func _coerce_(value: Value, env: Scope) throws -> SwiftType { // returns a 2-item tuple
        // try value.asPair()
        throw NotImplementedError() // TO DO: implement
    }
    
    
    func wrap(value: SwiftType, env: Scope) throws -> Value { // TO DO: is this right?
        return try Pair(self.left.wrap(value.0, env: env), self.right.wrap(value.1, env: env))
    }

}


class PairCoercion<KeyCoercion: CoercionProtocol, ValueCoercion: CoercionProtocol
        where KeyCoercion: Coercion, KeyCoercion.SwiftType: Value, ValueCoercion: Coercion, ValueCoercion.SwiftType: Value>: TuplePairCoercion<KeyCoercion, ValueCoercion> {
    
    func _coerce_(value: Value, env: Scope) throws -> Pair {
        return try self.wrap(super._coerce_(value, env: env), env: env) as! Pair
    }
}

//**********************************************************************


class TextCoercionBase: Coercion { // implements logic common to both native and primitive coercions
    
    let nonEmpty: Bool
    
    init(nonEmpty: Bool = false) { // TO DO: eventually add pattern match option
        self.nonEmpty = nonEmpty
    }
    
    override func defaultValue(env: Scope) throws -> Value { return Text("") }
    
    func _coerce_(value: Value, env: Scope) throws -> Text {
        let newValue = try value._expandAsText_(env, returnType: self)
        if self.nonEmpty && newValue.string == "" { throw CoercionError(value: value, coercion: self, description: "Empty text is not allowed.") }
        return newValue
    }
}


class TextCoercion: TextCoercionBase, CoercionProtocol {
    typealias SwiftType = Text
}


class StringCoercion: TextCoercionBase, CoercionProtocol {
    typealias SwiftType = String
    
    func _coerce_(value: Value, env: Scope) throws -> SwiftType {
        return try super._coerce_(value, env: env).string
    }
    
    func wrap(rawValue: SwiftType, env: Scope) throws -> Value {
        if self.nonEmpty && rawValue == "" { throw CoercionError(value: Text(rawValue), coercion: self, description: "Empty text is not allowed.") }
        return Text(rawValue)
    }
}




// TO DO: need ScalarCoercion; need [Scalar.?]'normalize' option for converting all scalars to same type




// TO DO: suspect this needs split into Integer and FloatingPoint subclasses, each of which takes exact ReturnType; also, what about SwiftType=Scalar? (that in itself could be problematic, since Scalar is a mismash of numbers and non-numbers; also need to give more thought to how to support mixed int+double calculations)



/*
class NumberCoercion { // TO DO: this should treat Int and Double as interchangeable, with `whole numbers only` as an optional constraint; that will avoid any problems where e.g. the first item in a list is a number with an internal .Integer representation and the second is a number with an internal .FloatingPoint representation (or vice-versa for that matter, as we don't want to convert Ints to Doubles unless/until necessary either, due to potential loss of precision in 64-bit)

// TO DO: need to eval value (e.g. if it's a command/expression), and eval in turn needs self, environment, and[?] coercion (e.g. List(Text) coercion would ideally be checked against return type of last proc [if it has one], allowing soft typing and, where required type ⊇ return type, eliminating need to apply coercion to results at all [or at least simplifying the 'coercion' operation, e.g. in cases where result is consumed by primitive code, only an unbox call would be required])

// note: will need two inits - one that takes Ints, one that takes Doubles; one possibility might be to put range check on Scalar, and just pass min and max as Scalars too (that means less wrapping/unwrapping when constructing this coercion via command)

func _coerce_(value: Value, env: Scope) throws -> Value {
// TO DO: eval
}

// given a text value, trim leading whitespace, call readDecimalNumber, check any remaining chars are whitespace only, and

// optional constraints: min and/or max

// returns: Text

}

*/


//**********************************************************************
//

// TO DO: need NumberCoercion that calls _expandAsText_ and checks result's range, returning SwiftType=Text


class ScalarCoercion: Coercion, CoercionProtocol {
    
    typealias SwiftType = Scalar
    
    let min: Scalar?
    let max: Scalar?
    let rangeConstraint: SwiftType throws -> Bool // if min and or max is given, checks that given value falls within those limits // TO DO: there probably isn't any benefit to this over using a couple of guards; plus min and max still need to be stored for display purposes and to generate corresponding coercion command
    
    init(min: SwiftType! = nil, max: SwiftType! = nil) { // note: for ints, could use Int.min and Int.max as defaults, eliminating need for switch
        self.min = min
        self.max = max
        switch (min,max) {
        case (nil,nil): self.rangeConstraint = {n in true}
        case (nil,_):   self.rangeConstraint = {n in try n <= max}
        case (_,nil):   self.rangeConstraint = {n in try min <= n}
        default:        self.rangeConstraint = {n in try min <= n && n <= max}
        }
    }
    
    override func defaultValue(env: Scope) throws -> Value { return Text("0") }
    
    func _coerce_(value: Value, env: Scope) throws -> SwiftType {
        let rawValue: SwiftType = try value._expandAsText_(env, returnType: self).toScalar()
        if try !self.rangeConstraint(rawValue) { throw CoercionError(value: value, coercion: self, description: "Out of range.") }
        return rawValue
    }
    
    func wrap(rawValue: SwiftType, env: Scope) throws -> Value {
        if try !self.rangeConstraint(rawValue) { throw CoercionError(value: Text(String(rawValue)), coercion: self, description: "Out of range.") }
        let text = Text(rawValue.literalRepresentation())
        text.annotations.append(rawValue)
        return text
    }
}

// Swift primitives


class IntCoercion: Coercion, CoercionProtocol {
    
    typealias SwiftType = Int
    
    let min: Int?
    let max: Int?
    let rangeConstraint: SwiftType->Bool // if min and or max is given, checks that given value falls within those limits // TO DO: there probably isn't any benefit to this over using a couple of guards; plus min and max still need to be stored for display purposes and to generate corresponding coercion command
    
    init(min: SwiftType! = nil, max: SwiftType! = nil) { // note: for ints, could use Int.min and Int.max as defaults, eliminating need for switch
        self.min = min
        self.max = max
        switch (min,max) {
        case (nil,nil): self.rangeConstraint = {n in true}
        case (nil,_):   self.rangeConstraint = {n in n <= max}
        case (_,nil):   self.rangeConstraint = {n in min <= n}
        default:        self.rangeConstraint = {n in min <= n && n <= max}
        }
    }
    
    override func defaultValue(env: Scope) throws -> Value { return Text("0") }
    
    func _coerce_(value: Value, env: Scope) throws -> SwiftType {
        let rawValue: SwiftType = try value._expandAsText_(env, returnType: self).toScalar().toInt()
        if !self.rangeConstraint(rawValue) { throw CoercionError(value: value, coercion: self, description: "Out of range.") }
        return rawValue
    }
    
    func wrap(rawValue: SwiftType, env: Scope) throws -> Value {
        let scalar = Scalar(rawValue)
        let text = Text(scalar.literalRepresentation()) // TO DO: add convenience constructor to Text that takes Scalar and annotates automatically
        text.annotations.append(scalar)
        if !self.rangeConstraint(rawValue) { throw CoercionError(value: text, coercion: self, description: "Out of range.") }
        return text
    }
}


class DoubleCoercion: Coercion, CoercionProtocol {
    
    typealias SwiftType = Double
    
    let min: Double?
    let max: Double?
    let rangeConstraint: SwiftType->Bool // if min and or max is given, checks that given value falls within those limits // TO DO: there probably isn't any benefit to this over using a couple of guards; plus min and max still need to be stored for display purposes and to generate corresponding coercion command
    
    init(min: SwiftType! = nil, max: SwiftType! = nil) {
        self.min = min
        self.max = max
        switch (min,max) {
        case (nil,nil): self.rangeConstraint = {n in true}
        case (nil,_):   self.rangeConstraint = {n in n <= max}
        case (_,nil):   self.rangeConstraint = {n in min <= n}
        default:        self.rangeConstraint = {n in min <= n && n <= max}
        }
    }
    
    override func defaultValue(env: Scope) throws -> Value { return Text("0.0") }
    
    func _coerce_(value: Value, env: Scope) throws -> SwiftType {
        let rawValue = try value._expandAsText_(env, returnType: self).toScalar().toDouble()
        if !self.rangeConstraint(rawValue) || rawValue == Double.infinity { throw CoercionError(value: value, coercion: self, description: "Out of range.") }
        return rawValue
    }
    
    func wrap(rawValue: SwiftType, env: Scope) throws -> Value {
        let scalar = Scalar(rawValue)
        let text = Text(scalar.literalRepresentation())
        text.annotations.append(scalar)
        if !self.rangeConstraint(rawValue) { throw CoercionError(value: text, coercion: self, description: "Out of range.") }
        return text
    }
}



class ArrayCoercion<ItemCoercion: CoercionProtocol where ItemCoercion: Coercion>: Coercion, CoercionProtocol {
    
    typealias SwiftType = [ItemCoercion.SwiftType]
    
    let itemType: ItemCoercion
    let min: Int
    let max: Int
    
    init(itemType: ItemCoercion, min: Int = 0, max: Int = Int.max) {
        self.itemType = itemType
        self.min = min
        self.max = max
    }
    
    override func defaultValue(env: Scope) throws -> Value { return List() }

    func _coerce_(value: Value, env: Scope) throws -> SwiftType {
        throw NotImplementedError()
 //       let rawValue = try value._expandAsText_(env, returnType: self).toScalar().toDouble()
 //       if !self.rangeConstraint(rawValue) || rawValue == Double.infinity { throw CoercionError(value: value, coercion: self, description: "Out of range.") }
 //       return rawValue
    }
}

extension ArrayCoercion { // deep-wrap rawValue array when it contains non-Value elements (this is recursive so may take some time on large collections of Swift values) // TO DO: might be worth shallow-wrapping large data structures and only wrap individual items if/when they are actually used

    func wrap(rawValue: SwiftType, env: Scope) throws -> Value {
        return List(items: try rawValue.map{try self.itemType.wrap($0, env: env)}, itemType: self.itemType) // TO DO: should annotated type always be converted to fully native Coercion? or can/should that be left till first time it's actually used?
    }
}

extension ArrayCoercion where ItemCoercion.SwiftType: Value { // shallow-wrap rawValue array when it contains Value elements

    func wrap(rawValue: SwiftType, env: Scope) throws -> Value {
        return List(items: rawValue, itemType: self.itemType)
    }
    
}



class CoercionCoercion: Coercion, CoercionProtocol {
    
    typealias SwiftType = Coercion
    
    func _coerce_(value: Value, env: Scope) throws -> Coercion {
        throw NotImplementedError()
    }
}




