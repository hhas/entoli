//
//  errors.swift
//  entoli-run
//
//

// TO DO: suspect structs are poor choice; probably want to subclass EntoliError


// TO DO: chainable errors (c.f. SwiftAutomation's CommandError)

// TO DO: use exceptions for early returns (e.g. when a `return RESULT` command is encountered, which is intercepted by Proc.evaluate()/Script.evaluate(); or a runtime error is thrown, which may be intercepted by a `catch` clause; or `didNothing` occurs in `if`/`repeat` command which may be intercepted by `else` clause)


//**********************************************************************
// parse errors


class SyntaxError: Error, CustomStringConvertible {
    
    let description: String
    
    init(description: String = "Syntax error.") { // TO DO: also needs range + source
        self.description = description
    }
    
} // TO DO: how to store error info?

class EndOfCodeError: SyntaxError {}

class LeftOperandNotFoundError: SyntaxError {}

class MalformedNumericError: SyntaxError {}



//**********************************************************************


//struct BadValue: ErrorType {}
struct BadArgument: Error, CustomStringConvertible {
    let description: String
}


typealias MismatchedField = BadArgument // TO DO: actual type?




struct EvaluationError: Error, CustomStringConvertible {
    let description: String // TO DO: separate message and `var description: String { return "\(type(of:self)): \(self.message)" }`
}


enum NameSearchLocation: CustomStringConvertible {
    case inScope(Scope)
    case inValue(Value)
    
    var description: String {
        switch self {
        case .inScope(let scope):
            return "in scope: \(scope)"
        case .inValue(let value):
            return "in value: \(value)"
        }
    }
}


struct NameNotFoundError: Error, CustomStringConvertible {
    let name: Name
    let location: NameSearchLocation
    let message: String? // TO DO: rename `message`
    
    init(name: Name, scope: Scope, description: String? = nil) {
        self.name = name
        self.location = .inScope(scope)
        self.message = description
    }
    init(name: Name, value: Value, description: String? = nil) {
        self.name = name
        self.location = .inValue(value)
        self.message = description
    }
    // TO DO: when generating detailed error messages, this should search scope chain/value's contents and generate a list of fuzzy matches as 'Did you mean...' suggestions
    
    var description: String {
        return "\(type(of:self)): \(self.name) not found \(self.location)"
    }
}



struct CastError: Error { // thrown by toVALUE() methods
    let value: Value
    let type: Value.Type
}


struct ConstraintError: Error {
    let value: Any // TO DO: what should this be? (e.g. enum of Value, Scalar, Primitive?)
    let constraint: Constraint?
    let description: String? // TO DO: rename message
    // TO DO: should this also capture env:Scope?
    
    init(value: Any, constraint: Constraint? = nil, description: String? = nil) {
        self.value = value
        self.constraint = constraint
        self.description = description
    }
}


enum ExpansionError: Error { // transient errors raised by Value._expandAsTYPE_() methods; should be caught and rethrown as permanent ConstraintError by Value.evaluate() if not handled in meantime (e.g. `DefaultValue.coerce()` will catch `.nullValue` and return a default value instead)
    case nullValue // `Null` value always throws this on expansion; may be intercepted by DefaultValue/MayBeNothing/MayBeNil<T>
    case unsupportedType // can't coerce to the specified type (e.g. lossy coercions such as List->Text are always disallowed)
    case failedConstraint(String) // value coerced to the required type but failed an additional constraint requirement (e.g. List contained more than ListConstraint.max items) // TO DO: how best to pass error details? as string, or as a more complex introspectable object?
}




func fatalNotYetImplemented(_ object: Any, _ methodName: String, _ message: String = "") -> Never  {
    fatalError("\(type(of: object)) does not yet implement \(methodName). \(message)")
}



struct NotSupportedError: Error {} // not an allowed operation



struct ScopeError: Error, CustomStringConvertible { // locked slot, name not found, etc
    let description: String
}


struct ImplementationError: Error, CustomStringConvertible { // internal bug // TO DO: better just to use `assert`?
    let description: String
}


struct ProcedureError: Error {
    let name: String
    let arguments: [Value]
    let commandScope: Scope
    let procedureScope: Scope
    let originalError: Error
}



