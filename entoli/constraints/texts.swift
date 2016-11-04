//
//  constraints/text.swift
//  entoli
//
//


//**********************************************************************


class TextConstraintBase: Constraint { // implements logic common to both native and primitive coercions
    
    let nonEmpty: Bool
    
    init(nonEmpty: Bool = false) { // TO DO: eventually add pattern match option
        self.nonEmpty = nonEmpty
    }
    
    override func defaultValue(_ env: Scope) throws -> Value { return Text("") }
    
    func _coerce(_ value: Value, env: Scope) throws -> Text {
        let newValue = try value._expandAsText_(env)
        if self.nonEmpty && newValue.string == "" {
            throw ConstraintError(value: value, constraint: self, description: "Empty text is not allowed.")
        }
        return newValue
    }
}


//


class TextConstraint: TextConstraintBase, SwiftCast, NativeConstraint {
    
    typealias SwiftType = Text
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try super._coerce(value, env: env)
    }
}


class StringConstraint: TextConstraintBase, SwiftCast {
    
    typealias SwiftType = String
    
    func _coerce_(_ value: Value, env: Scope) throws -> SwiftType {
        return try super._coerce(value, env: env).string
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        if self.nonEmpty && rawValue == "" {
            throw ConstraintError(value: Text(rawValue), constraint: self, description: "Empty text is not allowed.")
        }
        return Text(rawValue)
    }
    
    func toNative() -> NativeConstraint { return TextConstraint(nonEmpty: self.nonEmpty) }
}


// TO DO: need ScalarConstraint; need [Scalar.?]'normalize' option for converting all scalars to same type


// TO DO: suspect this needs split into Integer and FloatingPoint subclasses, each of which takes exact ReturnType; also, what about SwiftType=Scalar? (that in itself could be problematic, since Scalar is a mismash of numbers and non-numbers; also need to give more thought to how to support mixed int+double calculations)


//**********************************************************************
// numeric coercions

// TO DO: need NumberConstraint that calls _expandAsText_ and checks result's range, returning SwiftType=Text

/*
 class NumberConstraint { // TO DO: this should treat Int and Double as interchangeable, with `whole numbers only` as an optional constraint; that will avoid any problems where e.g. the first item in a list is a number with an internal .Integer representation and the second is a number with an internal .FloatingPoint representation (or vice-versa for that matter, as we don't want to convert Ints to Doubles unless/until necessary either, due to potential loss of precision in 64-bit)
 
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


class ScalarConstraint: Constraint, SwiftCast {
    
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
        if try !self.rangeConstraint(rawValue) { throw ConstraintError(value: value, constraint: self, description: "Out of range.") }
        return rawValue
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        if try !self.rangeConstraint(rawValue) { throw ConstraintError(value: Text(String(describing: rawValue)), constraint: self, description: "Out of range.") }
        let text = Text(rawValue.literalRepresentation())
        text.annotations.append(rawValue)
        return text
    }
}


// Swift primitives


class IntConstraint: Constraint, SwiftCast {
    
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
        if !self.rangeConstraint(rawValue) { throw ConstraintError(value: value, constraint: self, description: "Out of range.") }
        return rawValue
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        let scalar = Scalar(rawValue)
        let text = Text(scalar.literalRepresentation()) // TO DO: add convenience constructor to Text that takes Scalar and annotates automatically
        text.annotations.append(scalar)
        if !self.rangeConstraint(rawValue) { throw ConstraintError(value: text, constraint: self, description: "Out of range.") }
        return text
    }
}


class DoubleConstraint: Constraint, SwiftCast {
    
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
        if !self.rangeConstraint(rawValue) || rawValue == Double.infinity { throw ConstraintError(value: value, constraint: self, description: "Out of range.") }
        return rawValue
    }
    
    func wrap(_ rawValue: SwiftType, env: Scope) throws -> Value {
        let scalar = Scalar(rawValue)
        let text = Text(scalar.literalRepresentation())
        text.annotations.append(scalar)
        if !self.rangeConstraint(rawValue) { throw ConstraintError(value: text, constraint: self, description: "Out of range.") }
        return text
    }
}


//**********************************************************************
// convenience constants; used to perform type conversions when no custom constraints (e.g. 'cannot be empty', min/max bounds) are required

// TO DO: should lose `g` prefixes and lowercase first char of type name for consistency with Swift style guide

let gIntConstraint = IntConstraint()
let gDoubleConstraint = DoubleConstraint()
let gScalarConstraint = ScalarConstraint()

let gStringConstraint = StringConstraint()
let gTextConstraint = TextConstraint()



