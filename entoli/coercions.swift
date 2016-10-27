//
//  coercions.swift
//  entoli-run
//
//

// TO DO: split into separate files for easier maintenance, one for base classes, and one [or more] for each value type (e.g. Text might be 2 files, one of which covers numerical text and the other everything else)


// TO DO: only implement `wrap` methods on NativeCoercions? this'd simplify boxing and tagging, since the NativeCoercion need only attach itself to the new Value, whereas a primitive coercion needs to convert itself to a NativeCoercion and attach that. This does mean reworking the whole wrap API so that one or more non-generic wrap methods can be defined on a single NativeCoercion, e.g. `TextCoercion` would implement `wrap(_:Int,env:Scope)->Value`,`wrap(_:Double,env:Scope)->Value`,`wrap(_:String,env:Scope)->Value`. PROBLEM: ListCoercion require items to be boxed as well, but its `itemType` is dynamically bound as NativeCoercion so `List.wrap()` method can't get ItemType.SwiftType. (Also, arbitrary union coercions are going to be problematic however they're done so will probably require values to be explicitly boxed before they're returned.)


// TO DO: implement `description` methods (simplest would be to implement `toCommand()` for all coercions, then call that to obtain the corresponding constructor command and render that) [caveat we need to iron out `as` operator's behavior and/or install standalone coercion procs, which in turn may influence how coercion commands are written; not to mention some coercions, e.g. `editable`, `optional` will have operators as well]



// TO DO: how to describe - and unpack - arguments whose types are related to each other? e.g. in scalar operators, both operands need to be same primitive type (Int OR Double); in numeric operations, `+` and `-` require two numbers with same unit types (i.e. identical or inter-convertible, e.g. `2m + 3m`, `2m + 300cm`) while `*` and `/` require one numeric and one scalar to produce linear result (e.g. `4m / 2 => 2m`) or (if supported) might also accept two numerics (e.g. `2m * 3m => 6sqm`); this suggests current unpacking process - where each record field is matched and evaluated in turn - is only suitable for simple use cases, and a more sophisticated option where all fields are matched first, then grouped according to relationships and then further matched as groups to determine appropriate/optimal coercion[s] to apply to them (e.g. `&` operator needs to coerce both operands to same outer type, e.g. text&text or list&list or record&record); plus such matching needs to be table- and/or matchfunc-driven so that it's fully extensible



// TO DO: would this stuff work better if `evaluate` entry point was on Coercion rather than Value? (e.g. ListCoercion really wants to tell ItemType to coerce to native, regardless of what primitive type it has; also, would passing a `type:Any.Type` arg to evaluate and letting runtime decide what coercion to apply work better?); bear in mind primitive procs need to have everything typechecked at compile-time and also want everything to be hardwired for easy conversion to static Swift code, so table-based lookup is non-optimal there


//**********************************************************************
// Coercion protocol
//
// Defines methods that all coercions must implement, and provides default implementations for some where appropriate.
// Important: all coercion subclasses must inherit from Coercion base class, and add SwiftCast once API-complete.
// (This is a pain, but it keeps Swift's generics system happy when bridging between entoli Values and Swift types
// [Int, String, Array<...>, etc.], which in turn simplifies and standardizes glue code for primitive procedures.)



class Coercion: Value {
    
    // TO DO: Coercions should be Hashable and Equatable, allowing values to cache previously coerced representations of themselves, in addition to their tagged type(s)
    
    override func toCommand() throws -> Command { // all concrete subclasses must override this (unlike _expand..._, converts without evaluation)
        // TO DO: [also] implement _expandAsCommand_? (TBH, not sure how coercions should behave as values; suspect they need a Proc [sub]class for constructing them; _expandAsCommand_ would only be useful in metaprogramming for converting coercion values back to the commands that constructed them)
        fatalNotYetImplemented(self, #function) // TO DO: is this appropriate (all coercion classes _should_ be able to provide their corresponding constructor command)? or should it just throw an error if a particular coercion object can't provide a native constructor command? (another possibility is to return a command with an opaque value: that way it can still be used as a command, returning itself when command is evaled; just not converted to literal code)
    }
    
    override func _expandAsAny_(_ env: Scope) throws -> Value {
        return self
    }
    
    var defersExpansion: Bool { return false } // determines if Thunk.evaluate() should force and return its thunked value, or call ReturnType._coerce_(self,...) and let it decide what it wants to do with the Thunk (e.g. DoNotEvaluate will return Thunk unchanged, ThunkCoercion with thunk it again) // IMPORTANT: only special-case NativeCoercions should ever return true (e.g. ThunkCoercion, ExpressionCoercion) [might want to add an assert for that]
    
    func defaultValue(_ env: Scope) throws -> Value {
        // called by DefaultValue coercion when no value is given and it doesn't contain a default value itself; this avoids the need to write `DefaultValue(type: textCoercion, value: Text(""))`,  `DefaultValue(type: intCoercion, value: Text("0"))`, etc. since obvious defaults for these types can be inferred. One downside is that there's no way to check additional constraints, e.g. `DefaultValue(type: IntCoercion(min:10))` will throw a runtime error; OTOH, so will `DefaultValue(type: IntCoercion(min:10), Text("0"))`, so the problem isn't specific to inferred defaults, just easier to miss. (This is also why it can't return SwiftType, since constraint checking is performed by _coercion_;.)
        throw ImplementationError(description: "\(self) coercion does not provide a standard default value, and no other default was specified.") // note: the `default` type command (commonly used to define optional parameters to native procedures) should ensure that a valid default always exists or error if not, so the only time this error should occur is if a primitive procedure's signature has forgotten to supply one (i.e. developer error)
    }

    func intersect<ReturnType>(_ returnType: ReturnType, env: Scope) -> ReturnType where ReturnType: Coercion, ReturnType: SwiftCast { // note: the ReturnType is needed as caller wants to call _coerce_
        print("WARNING: Coercion.intersect not implemented for \(self)")
        return returnType
    }
    
    
    // TO DO: `var nativeType: NativeCoercion {get}`; this will return self on NativeCoercions; for bridging coercions, it should construct and return the NativeCoercion equivalent (caching for reuse); alternatively, don't implement `wrap` on SwiftCast at all and require library devs to use NativeCoercions as primitive procs' return types
}



protocol SwiftCast {
    
    associatedtype SwiftType
    
    // TO DO: are these necessary? base classes already define them
    var description: String { get } // subclasses must return their literal constructor representation
    var defersExpansion: Bool { get }
    //func defaultValue(_ env: Scope) throws -> Value
    
    
    func intersect<ReturnType>(_ returnType: ReturnType, env: Scope) -> ReturnType where ReturnType: Coercion, ReturnType: SwiftCast // TO DO: what other set operations (`union`, `contains`, `==`, `is[Strict]SubsetOf`, `is[Strict]SupersetOf`) should be supported? note that `contains()` is needed to check that a Value meets Coercion's constraints without coercing it first (i.e. sum types need to check for an exact match first before trying to find a coerced match), and is further complicated by fact that collections may provide a partial exact match (e.g. a list of numbers partially matches a list of text in that both have the same container type, so should be given preferential treatment when trying coerced matches)
    
    // TO DO: `func toCommand() throws -> Command` should return Coercion value's constructor command, e.g. `TextCoercion.toCommand()` -> `text {...}`
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType // called by Value.evaluate(); other code should avoid calling this directly
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value // (this shouldn't normally require env, but it's supplied just in case); can be used to re-wrap the Swift value returned by Value.evaluate(), e.g. `try someType.wrap(someValue.evaluate(env, returnType: someType), env: env)` // TO DO: this would work better if there was a `typealias NativeType` to improve its return type; having both 'before' and 'after' types present may also prove useful elsewhere
}



extension SwiftCast where SwiftType: Value {
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType { // subclasses must override
        fatalNotYetImplemented(self, #function)
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value { // Value->Value coercions don't need to implement custom wrap() methods
        return rawValue
    }
}


protocol NativeCoercion {
    // TO DO: what does this need to expose?
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType // subclasses must override
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value // Value->Value coercions don't need to implement custom wrap() methods
    func defaultValue(_ env: Scope) throws -> Value
    var defersExpansion: Bool {get} // TO DO: might be more appropriate to provide `specialExpansion(value,env)->(value,returnNow)` which gets called by Value.evaluate() prior to/instead of it doing its own thing, in which case maybe add a SpecialExpansion protocol which DoNotExpand, ThunkCoercion, etc adopt; evaluate methods will then check for that protocol (alternatively, it might be simpler always to call specialExpansion which in most cases will be a no-op)
}

extension NativeCoercion {
    typealias SwiftType = Value
}


//**********************************************************************
//


class AnyValueCoercion: Coercion, SwiftCast, NativeCoercion { // by default, allows anything *except* gNullValue // TO DO: what name? also, how best to implement primitive equivalent? (ideally would be a generic that takes task-specific enum type defined by client code, but can't see how that would work; alternative is just to return Any)
    
    typealias SwiftType = Value // TO DO
    
    // TO DO: option to constrain to one or more specified native Coercion types (i.e. implicit union) e.g. `any [text, list, record]`; this'll probably need to be a list, since order is significant (also needs to do two passes: first to check for exact type match, second to try coercing; oh, and first pass should also check for best partial match for lists and records, since)
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try value._expandAsAny_(env)
    }
}



//


class DoNotEvaluate: Coercion, SwiftCast, NativeCoercion { // no-op; unlike AnyValueCoercion, which expands a value to its own choice of type, this immediately returns value without any evaluation; e.g. for use in primitive procedures that want to do their own thing
    
    typealias SwiftType = Value
    
    override var defersExpansion: Bool { return true }
    
    let type: Coercion
    
    init(type: Coercion = gAnyValueCoercion) { // for documentation purposes, constructor should take a Coercion object that will provide user with description of what's expected; this Coercion instance can always be used by primitive func if/when it needs to apply it itself
        self.type = type
    }
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return value
    }
}


typealias ExpressionCoercion = DoNotEvaluate // TO DO: need to decide namings


//**********************************************************************


class TextCoercionBase: Coercion { // implements logic common to both native and primitive coercions
    
    let nonEmpty: Bool
    
    init(nonEmpty: Bool = false) { // TO DO: eventually add pattern match option
        self.nonEmpty = nonEmpty
    }
    
    override func defaultValue(_ env: Scope) throws -> Value { return Text("") }
    
    func _coerce(_ value: Value, env: Scope) throws -> Text {
        let newValue = try value._expandAsText_(env)
        if self.nonEmpty && newValue.string == "" {
            throw CoercionError(value: value, coercion: self, description: "Empty text is not allowed.")
        }
        return newValue
    }
}


class TextCoercion: TextCoercionBase, SwiftCast, NativeCoercion {
    
    typealias SwiftType = Text
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try super._coerce(value, env: env)
    }
}


class StringCoercion: TextCoercionBase, SwiftCast {
    
    typealias SwiftType = String
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try super._coerce(value, env: env).string
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        if self.nonEmpty && rawValue == "" {
            throw CoercionError(value: Text(rawValue), coercion: self, description: "Empty text is not allowed.")
        }
        return Text(rawValue)
    }
}


// TO DO: need ScalarCoercion; need [Scalar.?]'normalize' option for converting all scalars to same type


// TO DO: suspect this needs split into Integer and FloatingPoint subclasses, each of which takes exact ReturnType; also, what about SwiftType=Scalar? (that in itself could be problematic, since Scalar is a mismash of numbers and non-numbers; also need to give more thought to how to support mixed int+double calculations)


//**********************************************************************


class NameCoercion: Coercion, SwiftCast, NativeCoercion {
    
    typealias SwiftType = Name
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try value._expandAsName_(env) // TO DO: not sure about this (it doesn't expand, but rather typechecks value to see if it's a Name and throws coercion error if not; TBH name literals are a huge pain since they're ambiguous with commands, and must sometimes be treated as name literals - e.g. record keys - and other times as arg-less commands, e.g. record values and most other contexts; worse, a list of name values won't roundtrip when formatted as literal then reparsed [unless formatter knows to e.g. wrap them in `as name` casts])
    }
}


class NameKeyStringCoercion: Coercion, SwiftCast {
    
    typealias SwiftType = String
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try value._expandAsName_(env).keyString
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        return Name(rawValue)
    }
}


class CommandCoercion: Coercion, SwiftCast, NativeCoercion {
    
    typealias SwiftType = Command
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try value._expandAsCommand_(env)
    }
}


//**********************************************************************
// numeric coercions

// TO DO: need NumberCoercion that calls _expandAsText_ and checks result's range, returning SwiftType=Text

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


class ScalarCoercion: Coercion, SwiftCast {
    
    typealias SwiftType = Scalar
    
    let min: Scalar?
    let max: Scalar?
    let rangeConstraint: (SwiftType) throws -> Bool // if min and or max is given, checks that given value falls within those limits // TO DO: there probably isn't any benefit to this over using a couple of guards; plus min and max still need to be stored for display purposes and to generate corresponding coercion command
    
    // TO DO: Swift poops on itself when implicitly unboxed optionals are declared in type sigs, so make sure these are replaced
    
    init(min: SwiftType? = nil, max: SwiftType? = nil) { // note: for ints, could use Int.min and Int.max as defaults, eliminating need for switch
        self.min = min
        self.max = max
        switch (min,max) {
        case (nil,nil): self.rangeConstraint = {n in true}
        case (nil,_):   self.rangeConstraint = {n in try n <= max!}
        case (_,nil):   self.rangeConstraint = {n in try min! <= n}
        default:        self.rangeConstraint = {n in try min! <= n && n <= max!}
        }
    }
    
    override func defaultValue(_ env: Scope) throws -> Value { return Text("0") }
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        let rawValue: SwiftType = try value._expandAsText_(env).toScalar()
        if try !self.rangeConstraint(rawValue) { throw CoercionError(value: value, coercion: self, description: "Out of range.") }
        return rawValue
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        if try !self.rangeConstraint(rawValue) { throw CoercionError(value: Text(String(describing: rawValue)), coercion: self, description: "Out of range.") }
        let text = Text(rawValue.literalRepresentation())
        text.annotations.append(rawValue)
        return text
    }
}


// Swift primitives


class IntCoercion: Coercion, SwiftCast {
    
    typealias SwiftType = Int
    
    let min: SwiftType?
    let max: SwiftType?
    let rangeConstraint: (SwiftType)->Bool // if min and or max is given, checks that given value falls within those limits // TO DO: there probably isn't any benefit to this over using a couple of guards; plus min and max still need to be stored for display purposes and to generate corresponding coercion command
    
    init(min: SwiftType? = nil, max: SwiftType? = nil) { // note: for ints, could use Int.min and Int.max as defaults, eliminating need for switch
        self.min = min
        self.max = max
        switch (min,max) {
        case (nil,nil): self.rangeConstraint = {n in true}
        case (nil,_):   self.rangeConstraint = {n in n <= max!}
        case (_,nil):   self.rangeConstraint = {n in min! <= n}
        default:        self.rangeConstraint = {n in min! <= n && n <= max!}
        }
    }
    
    override func defaultValue(_ env: Scope) throws -> Value { return Text("0") }
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        let rawValue: SwiftType = try value._expandAsText_(env).toScalar().toInt()
        if !self.rangeConstraint(rawValue) { throw CoercionError(value: value, coercion: self, description: "Out of range.") }
        return rawValue
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        let scalar = Scalar(rawValue)
        let text = Text(scalar.literalRepresentation()) // TO DO: add convenience constructor to Text that takes Scalar and annotates automatically
        text.annotations.append(scalar)
        if !self.rangeConstraint(rawValue) { throw CoercionError(value: text, coercion: self, description: "Out of range.") }
        return text
    }
}


class DoubleCoercion: Coercion, SwiftCast {
    
    typealias SwiftType = Double
    
    let min: SwiftType?
    let max: SwiftType?
    let rangeConstraint: (SwiftType)->Bool // if min and or max is given, checks that given value falls within those limits // TO DO: there probably isn't any benefit to this over using a couple of guards; plus min and max still need to be stored for display purposes and to generate corresponding coercion command
    
    init(min: SwiftType? = nil, max: SwiftType? = nil) {
        self.min = min
        self.max = max
        switch (min,max) {
        case (nil,nil): self.rangeConstraint = {n in true}
        case (nil,_):   self.rangeConstraint = {n in n <= max!}
        case (_,nil):   self.rangeConstraint = {n in min! <= n}
        default:        self.rangeConstraint = {n in min! <= n && n <= max!}
        }
    }
    
    override func defaultValue(_ env: Scope) throws -> Value { return Text("0.0") }
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        let rawValue = try value._expandAsText_(env).toScalar().toDouble()
        if !self.rangeConstraint(rawValue) || rawValue == Double.infinity { throw CoercionError(value: value, coercion: self, description: "Out of range.") }
        return rawValue
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        let scalar = Scalar(rawValue)
        let text = Text(scalar.literalRepresentation())
        text.annotations.append(scalar)
        if !self.rangeConstraint(rawValue) { throw CoercionError(value: text, coercion: self, description: "Out of range.") }
        return text
    }
}


//**********************************************************************
// boolean coercions // TO DO: how this is implemented will depend on whether Boolean tests return traditional `true`/`false` names or Icon-style success/failure (if the latter, then `failed test` will be a special name similar to `nothing` and original value can be returned as-is)


// TO DO: BooleanCoercion that returns Value


class BoolCoercion: Coercion, SwiftCast {
    
    typealias SwiftType = Bool
    
    override func defaultValue(_ env: Scope) throws -> Value { return Text("FALSE") } // TO DO: fix once boolean representations and behavior are decided
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        fatalNotYetImplemented(self, #function) // TO DO: ditto
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        return Text(rawValue ? "TRUE" : "FALSE") // TO DO: ditto
    }
}


//**********************************************************************


// TO DO: RecordCoercion


class ArrayCoercion<ItemCoercion>: Coercion, SwiftCast where ItemCoercion: Coercion, ItemCoercion: SwiftCast {
    
    typealias SwiftType = [ItemCoercion.SwiftType]
    
    let itemType: ItemCoercion
    let min: Int
    let max: Int
    
    init(itemType: ItemCoercion, min: Int = 0, max: Int = Int.max) {
        self.itemType = itemType
        self.min = min
        self.max = max
    }
    
    override func defaultValue(_ env: Scope) throws -> Value { return List() }

    // func _expandAsArray_<ItemType>(_ env: Scope, itemType: ItemType) throws -> [ItemType.SwiftType] where ItemType: Coercion, ItemType: SwiftCast

    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType { // TO DO: implement
        fatalNotYetImplemented(self, #function)
        // TO DO: use `toArray()->[Value]` instead, then expand items here?
        /*
        let result = try value._expandAsArray_(env: env, itemType: self.itemType) // compiler complains it can't infer itemType
        if result.count < self.min {
            throw CoercionError(value: value, coercion: self, description: "Expected at least \(self.min) items but found \(result.count)") // TO DO: `@inline(__always) pluralize()` helper function
        } else if result.count > self.max {
            throw CoercionError(value: value, coercion: self, description: "Expected at most \(self.max) items but found \(result.count)")
        }
        return result
        */
    }
}

extension ArrayCoercion { // deep-wrap rawValue array when it contains non-Value elements (this is recursive so may take some time on large collections of Swift values) // TO DO: might be worth shallow-wrapping large data structures and only wrap individual items if/when they are actually used

    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        // TO DO: need to catch and rethrow temporary errors (e.g. NullCoercionError) as permanent coercion errors; ditto elsewhere
        return List(items: try rawValue.map{try self.itemType.wrap($0, env: env)}) //, itemType: self.itemType) // TO DO: need NativeCoercion for itemType parameter // TO DO: should annotated type always be converted to fully native Coercion? or can/should that be left till first time it's actually used?
    }
}

extension ArrayCoercion where ItemCoercion.SwiftType: Value { // shallow-wrap rawValue array when it contains Value elements

    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        return List(items: rawValue) //, itemType: self.itemType) // TO DO: need NativeCoercion for itemType parameter
    }
    
}


class ListCoercion: Coercion, SwiftCast, NativeCoercion {
    
    typealias SwiftType = List
    
    let itemType: NativeCoercion
    let min: Int
    let max: Int
    
    init(itemType: NativeCoercion, min: Int = 0, max: Int = Int.max) {
        self.itemType = itemType
        self.min = min
        self.max = max
    }
    
    override func defaultValue(_ env: Scope) throws -> Value { return List() }
    
    // func _expandAsArray_<ItemType>(_ env: Scope, itemType: ItemType) throws -> [ItemType.SwiftType] where ItemType: Coercion, ItemType: SwiftCast
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType { // TO DO: implement
        return try value._expandAsList_(env, itemType: self.itemType)
    }
    
    func wrap<ItemType>(_ rawValue: [ItemType], env: Scope) throws -> Value {
        fatalError("TODO: how to wrap items?")
    }
}


//**********************************************************************
//


class TuplePairCoercion<KeyCoercion, ValueCoercion>: Coercion, SwiftCast
                        where KeyCoercion: Coercion, KeyCoercion: SwiftCast, ValueCoercion: Coercion, ValueCoercion: SwiftCast {
    
    typealias SwiftType = (KeyCoercion.SwiftType, ValueCoercion.SwiftType)
    
    let left: KeyCoercion
    let right: ValueCoercion
    
    init(left: KeyCoercion, right: ValueCoercion) {
        self.left = left
        self.right = right
    }
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType { // returns a 2-item tuple
        // try value.asPair()
        fatalNotYetImplemented(self, #function) // TO DO: implement
    }
    
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value { // TO DO: is this right?
        return try Pair(self.left.wrap(rawValue.0, env: env), self.right.wrap(rawValue.1, env: env))
    }
    
}


class PairCoercion<KeyCoercion, ValueCoercion>: TuplePairCoercion<KeyCoercion, ValueCoercion>, NativeCoercion
                    where KeyCoercion: Coercion, KeyCoercion: SwiftCast, KeyCoercion.SwiftType: Value,
                            ValueCoercion: Coercion, ValueCoercion: SwiftCast, ValueCoercion.SwiftType: Value {
    
    typealias SwiftType = Pair
    
    func _coerce_(_ value: Value, env: Scope) throws -> Pair {
        return try self.wrap(super._coerce_(value, env: env), env: env) as! Pair
    }
}


//**********************************************************************
//


class TypeCoercion: Coercion, SwiftCast, NativeCoercion { // coerce value to a Coercion instance
    
    typealias SwiftType = Coercion
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        if let coercion = value as? Coercion { return coercion } // TO DO: this is no good, since a ProxyCoercion needs to be fully evaled to return actual Coercion instance
        
        // TO DO: can/should this call value.toCommand() and evaluate? (not sure about this)
        
        fatalNotYetImplemented(self, #function)
    }
}



class ParameterTypeCoercion: Coercion, SwiftCast, NativeCoercion {

    typealias SwiftType = ParameterType // RecordSignature
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try value._expandAsRecord_(env).toRecordSignature()
    }
}


//**********************************************************************
// coercion modifiers


class ThunkCoercion: Coercion, SwiftCast, NativeCoercion { // aka `lazy`
    
    typealias SwiftType = Thunk
    
    let type: Coercion
    
    init(type: Coercion = gAnyValueCoercion) { // TO DO: just how necessary is it to include type here? e.g. lets us specify e.g. `as lazy (list of text)`
        self.type = type
    }
    
    override var defersExpansion: Bool { return true }
    
    override func defaultValue(_ env: Scope) throws -> Value { return try self.type.defaultValue(env) }
    
    func _coerce_(_ value: Value, env: Scope) throws -> Value {
        return Thunk(value: value, env: env, type: self.type) // TO DO: context?
    }
    
}



class MayBeNothing<ReturnType>: Coercion, SwiftCast, NativeCoercion
                where ReturnType: Coercion, ReturnType: SwiftCast, ReturnType.SwiftType: Value {
    
    typealias SwiftType = Value
    
    let type: ReturnType
    
    init(type: ReturnType) {
        self.type = type
    }
    
    override var defersExpansion: Bool { return self.type.defersExpansion }
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        do {
            return try value.evaluate(env, returnType: self.type)
        } catch ExpansionError.nullValue {
            return gNullValue
        }
    }
}


class MayBeNil<ReturnType>: Coercion, SwiftCast where ReturnType: Coercion, ReturnType: SwiftCast {
    
    typealias SwiftType = Optional<ReturnType.SwiftType>
    
    let type: ReturnType
    
    init(type: ReturnType) {
        self.type = type
    }
    
    override var defersExpansion: Bool { return self.type.defersExpansion }
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        do {
            return Optional.some(try value.evaluate(env, returnType: self.type))
        } catch ExpansionError.nullValue {
            return Optional.none
        }
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        switch rawValue {
        case .some(let v): return try self.type.wrap(v, env: env)
        case .none:               return gNullValue
        }
    }
}


//

// TO DO: this should use NativeCoercion where ReturnType.SwiftType:Value

class DefaultValue<ReturnType>: Coercion, SwiftCast where ReturnType: Coercion, ReturnType: SwiftCast {
    
    typealias SwiftType = ReturnType.SwiftType
    
    let type: ReturnType
    let value: Value?
    
    init(type: ReturnType, value: Value? = nil) {
        self.type = type
        self.value = value
    }
    
    override var defersExpansion: Bool { return self.type.defersExpansion }
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        var expandedValue: SwiftType
        do {
            expandedValue = try value.evaluate(env, returnType: self.type)
        } catch ExpansionError.nullValue {
            let defaultValue: Value
            if self.value == nil {
                do {
                    defaultValue = try self.type.defaultValue(env)
                } catch {
                    throw CoercionError(value: value, coercion: self, description: "No value was given, and no default was provided: \(error)")
                }
            } else {
                defaultValue = self.value!
            }
            do {
                expandedValue = try defaultValue.evaluate(env, returnType: self.type)
            } catch {
                throw CoercionError(value: value, coercion: self, description: "Couldn't use standard default: \(error)")
            }
        }
        return expandedValue
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        return try self.type.wrap(rawValue, env: env)
    }
}


// TO DO: ditto
class Precis<ReturnType>: Coercion, SwiftCast where ReturnType: Coercion, ReturnType: SwiftCast { // provides a custom description of Coercion object for documentation purposes
    
    typealias SwiftType = ReturnType.SwiftType
    
    let type: ReturnType
    let _description: String
    
    override var defersExpansion: Bool { return self.type.defersExpansion }
    
    override var description: String { return self._description }
    
    init(type: ReturnType, description: String = "") {
        self.type = type
        self._description = description
    }
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try value.evaluate(env, returnType: self.type)
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        return try self.type.wrap(rawValue, env: env)
    }
}


//**********************************************************************
// commonly used Coercions, predefined for convenience

// TO DO: rename `asAnyValue`, `asText`, `asString`, etc? (need to decide naming convention for standard and special coercion classes, and their convenience constants; e.g. `cAnyValue`, `cText`, etc. would still be preferable to `gAnyValueCoercion`, `gTextCoercion`)

let gNoResult = Precis(type: gAnythingCoercion, description: "nothing") // TO DO: need to change this to use `NoResult` as it needs to evaluate normally, discard any result that might be given, and return gNullValue

let gDoNotEvaluate = DoNotEvaluate()

let gAnyValueCoercion = AnyValueCoercion() // any value except `nothing`

let gAnythingCoercion = MayBeNothing(type: gAnyValueCoercion) // any value including `nothing`

let gBoolCoercion = BoolCoercion()

let gIntCoercion = IntCoercion()
let gDoubleCoercion = DoubleCoercion()
let gScalarCoercion = ScalarCoercion()

let gStringCoercion = StringCoercion()
let gTextCoercion = TextCoercion()

let gNameCoercion = NameCoercion()
let gNameKeyStringCoercion = NameKeyStringCoercion()

let gCommandCoercion = CommandCoercion()

let gTypeCoercion = TypeCoercion()

let gParameterTypeCoercion = ParameterTypeCoercion()
let gReturnTypeCoercion = gTypeCoercion




