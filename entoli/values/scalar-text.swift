//
//  values/scalar.swift
//  entoli-run
//
//  Provides underlying Int/Double (or overflow) representation for Text values that represent numbers
//

import Darwin


enum Scalar { // represents an integer (as Swift Int) or decimal (as Swift Double) number; numbers that are valid but too large to represent using standard Swift types are held as strings
    
    case integer(Int)
    case floatingPoint(Double)
    case overflow(String, Any.Type) // TO DO: separate enums for int vs double overflows? also, what about UInt?
    
    init(_ n: Int) {
        self = .integer(n)
    }
    init(_ n: Double) {
        self = (n == Double.infinity) ? .overflow(String(n), Double.self) : .floatingPoint(n)
    }
    init(_ n: Int8) {
        self = .integer(Int(n))
    }
    init(_ n: Int16) {
        self = .integer(Int(n))
    }
    init(_ n: Int32) {
        self = .integer(Int(n))
    }
    init(_ n: Int64) {
        self = .integer(Int(n))
    }
    init(_ n: UInt8) {
        self = .integer(Int(n))
    }
    init(_ n: UInt16) {
        self = .integer(Int(n))
    }
    init(_ n: UInt32) {
        self = .integer(Int(n)) // TO DO: 32-bit Int compatibility
    }
    init(_ n: UInt64) { // TO DO: better UInt compatibility
        self = n > UInt64(Int.max) ? .overflow(String(n), UInt64.self) : .integer(Int(n))
    }
    init(_ n: Float) {
        self = (n == Float.infinity) ? .overflow(String(n), Float.self) : .floatingPoint(Double(n))
    }
    
    // initializers primarily intended for use by scalar parsefuncs below // TO DO: should these *only* be used by numeric parsefuncs?
    // note: these constructors use Swift's own Int(String)/Double(String) constructors, thus underscores may be used as thousands separators, leading/trailing whitespace is not allowed, int constructor doesn't accept decimals, double constructor only accepts period (`.`) as decimal separator, fractional exponents aren't allowed, etc.
    
    // TO DO: init(number code: String,...) that chooses best internal representation? this basically means calling readDecimalNumber() parsefunc, so not sure how useful that is really, given that these inits only exist for parsefuncs' use in the first palce
    
    init(int code: String, isNegative: Bool, radix: Int = 10) throws {
        if let number = Int(code, radix: radix) {
            self = .integer(isNegative ? -number : number)
        } else if radix == 10 && Double(code) != nil {
            self = .overflow(String(code), Int.self)
        } else {
            throw EvaluationError(description: "Not a base-\(radix) int: \(code)")
        }
    }
    
    init(double code: String, isNegative: Bool) throws {
        guard let number = Double(code) else { throw EvaluationError(description: "Not a double: \(code)") } // TO DO: how to distinguish .Overflow (e.g. if `code` is "1e500") from .NotNumber? (e.g. if `code` is empty or contains spaces or other invalid chars); note that Double(String) returns nil, not Double.infinity, upon overflow, which makes it hard to determine if it's Double overflow or malformed text
        self = .floatingPoint(isNegative ? -number : number)
    }
    
    
    // unwrap Swift primitives
    
    func toInt() throws -> Int {
        switch self {
        case .integer(let n):                           return n
        case .floatingPoint(let n) where n.truncatingRemainder(dividingBy: 1) == 0:    return Int(n)
        case .overflow(_, let t) where t is Int.Type:   throw ConstraintError(value: self, description: "Number is too large to use: \(self.literalRepresentation())")
        default:                                        throw ConstraintError(value: self, description: "Not a whole number: \(self.literalRepresentation())")
        }
    }
    
    func toDouble() throws -> Double {
        switch self {
        case .integer(let n):       return Double(n)
        case .floatingPoint(let n): return n
        default:                    throw ConstraintError(value: self, description: "Number is too large to use: \(self.literalRepresentation())")
        }
    }
    
    
    // overloaded generic-friendly version of toInt/toDouble; used by numeric coercions' generic base class
    
    private func _toInt(_ min: Int, _ max: Int) throws -> Int {
        let n = try self.toInt()
        if n < min || n > max { throw ConstraintError(value: self, description: "Whole number is too large to use: \(self.literalRepresentation())") }
        return n
    }
    private func _toUInt(_ max: UInt) throws -> UInt {
        let n = try self.toInt()
        if n < 0 || UInt(n) > max { throw ConstraintError(value: self, description: "Whole number is too large to use: \(self.literalRepresentation())") }
        return UInt(n)
    }
    
    func toSwift() throws -> Int {
        return try self.toInt()
    }
    func toSwift() throws -> Int8 {
        return Int8(try self._toInt(Int(Int8.min), Int(Int8.max)))
    }
    func toSwift() throws -> Int16 {
        return Int16(try self._toInt(Int(Int16.min), Int(Int16.max)))
    }
    func toSwift() throws -> Int32 {
        return Int32(try self._toInt(Int(Int32.min), Int(Int32.max)))
    }
    func toSwift() throws -> Int64 {
        return Int64(try self.toInt())
    }
    func toSwift() throws -> UInt8 {
        return UInt8(try self._toUInt(UInt(UInt8.max)))
    }
    func toSwift() throws -> UInt16 {
        return UInt16(try self._toUInt(UInt(UInt16.max)))
    }
    func toSwift() throws -> UInt32 {
        return UInt32(try self._toUInt(UInt(UInt32.max)))
    }
    func toSwift() throws -> UInt64 { // note: this only covers bottom half of 0..<UInt64.max, which isn't ideal, but it remains to be seen how important and/or practical 64-bit unsigned ints are (e.g. they may be an issue when, say, interacting with some Cocoa APIs, in which case Scalar may need to include an .UnsignedInteger case just to ensure compatibility)
        return UInt64(try self._toUInt(UInt(UInt64.max)))
    }
    func toSwift() throws -> Float {
        let n = try Float(self.toDouble())
        if n == Float.infinity { throw ConstraintError(value: self, description: "Whole number is too large to use: \(self.literalRepresentation())") }
        return n
    }
    func toSwift(_ min: Double, _ max: Double) throws -> Double {
        return try self.toDouble()
    }
    
    //
    
    func literalRepresentation() -> String { // get canonical code representation (note: this is currently implemented as a method to allow for formatting options to be passed in future) // TO DO: check these representations are always correct
        switch self {
        case .integer(let n):
            return String(n)
        case .floatingPoint(let n):
            return String(n)
        case .overflow(let s, _):
            return s
        }
    }
    
    // TO DO: implement formattedRepresentation (custom/locale-specific) here? or does that logic belong solely in formatting command? (i.e. all Values should implement API for outputting pretty-printed code representation, but not sure if that API should support all formatting operations)
}



//**********************************************************************
// generic helper functions for basic arithmetic and numerical comparisons


func scalarArithmeticOperation(_ lhs: Scalar, _ rhs: Scalar, intOperator: ((Int,Int)->(Int,Bool))?, doubleOperator: (Double,Double)->Double) throws -> Scalar {
    switch (lhs, rhs) {
    case (.integer(let leftOp), .integer(let rightOp)):
        if let op = intOperator {
            let (result, isOverflow) = op(leftOp, rightOp)
            // TO DO: how best to deal with integer overflows? switch to Double automatically? (i.e. loses precision, but allows operation to continue)
            return isOverflow ? .overflow(String(doubleOperator(try lhs.toDouble(), try rhs.toDouble())), Int.self) : Scalar(result)
        } else {
            return try Scalar(doubleOperator(lhs.toDouble(), rhs.toDouble()))
        }
    default: // TO DO: this should be improved so that if one number is Int and the other is a Double that can be accurately represented as Int then Int-based operation is tried first; if that overflows then fall back to using Doubles; note that best way to do this may be to implement Scalar.toBestRepresentation() that returns .Integer/.FloatingPoint after first checking if the latter can be accurately represented as an Integer instead
        return try Scalar(doubleOperator(lhs.toDouble(), rhs.toDouble()))
    }
}

func scalarComparisonOperation(_ lhs: Scalar, _ rhs: Scalar, intOperator: (Int,Int)->Bool, doubleOperator: (Double,Double)->Bool) throws -> Bool {
    switch (lhs, rhs) {
    case (.integer(let leftOp), .integer(let rightOp)):
        return intOperator(leftOp, rightOp)
    default:
        return try doubleOperator(lhs.toDouble(), rhs.toDouble()) // TO DO: as above, use Int-based comparison where possible (casting an Int to Double is lossy in 64-bit, which may affect correctness of result when comparing a high-value Int against an almost equivalent Double)
        // TO DO: when comparing Doubles for equality, use almost-equivalence as standard? (e.g. 0.7*0.7=0.49 will normally return false due to rounding errors in FP math, which is likely to be more confusing to users than if the test is fudged)
    }
}


//**********************************************************************
// Arithmetic and comparison operators are defined on Scalar so that primitive procs can perform basic
// numerical operations without having to check or care about underlying representations (Int or Double).


typealias ScalarArithmeticFunction = (Scalar, Scalar) throws -> Scalar
typealias ScalarComparisonFunction = (Scalar, Scalar) throws -> Bool


func +(lhs: Scalar, rhs: Scalar) throws -> Scalar {
    return try scalarArithmeticOperation(lhs, rhs, intOperator: {(l:Int,r:Int) in l.addingReportingOverflow(r)}, doubleOperator: +)
}
func -(lhs: Scalar, rhs: Scalar) throws -> Scalar {
    return try scalarArithmeticOperation(lhs, rhs, intOperator: {(l:Int,r:Int) in l.subtractingReportingOverflow(r)}, doubleOperator: -)
}
func *(lhs: Scalar, rhs: Scalar) throws -> Scalar {
    return try scalarArithmeticOperation(lhs, rhs, intOperator: {(l:Int,r:Int) in l.multipliedReportingOverflow(by: r)}, doubleOperator: *)
}
func /(lhs: Scalar, rhs: Scalar) throws -> Scalar {
    return try scalarArithmeticOperation(lhs, rhs, intOperator: nil, doubleOperator: /)
}
//func %(lhs: Scalar, rhs: Scalar) throws -> Scalar { // TO DO: truncatingRemainder
//    return try scalarArithmeticOperation(lhs, rhs, intOperator: nil, doubleOperator: %)
//}
func pow(_ lhs: Scalar, rhs: Scalar) throws -> Scalar {
    return Scalar(try pow(lhs.toDouble(), rhs.toDouble()))
}
func integerDivision(_ lhs: Scalar, rhs: Scalar) throws -> Scalar {
    switch (lhs, rhs) {
    case (.integer(let leftOp), .integer(let rightOp)):
        return Scalar(leftOp / rightOp)
    default:
        let n = try (lhs / rhs).toDouble()
        return Scalar((n >= Double(Int.min) && n <= Double(Int.max)) ? Int(n) : lround(n))
    }
}



func <(lhs: Scalar, rhs: Scalar) throws -> Bool {
    return try scalarComparisonOperation(lhs, rhs, intOperator: <, doubleOperator: <)
}
func <=(lhs: Scalar, rhs: Scalar) throws -> Bool {
    return try scalarComparisonOperation(lhs, rhs, intOperator: <=, doubleOperator: <=)
}
func ==(lhs: Scalar, rhs: Scalar) throws -> Bool {
    return try scalarComparisonOperation(lhs, rhs, intOperator: ==, doubleOperator: ==)
}
func !=(lhs: Scalar, rhs: Scalar) throws -> Bool {
    return try scalarComparisonOperation(lhs, rhs, intOperator: !=, doubleOperator: !=)
}
func >(lhs: Scalar, rhs: Scalar) throws -> Bool {
    return try scalarComparisonOperation(lhs, rhs, intOperator: >, doubleOperator: >)
}
func >=(lhs: Scalar, rhs: Scalar) throws -> Bool {
    return try scalarComparisonOperation(lhs, rhs, intOperator: >=, doubleOperator: >=)
}




