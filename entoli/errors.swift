//
//  errors.swift
//  entoli-run
//
//

// TO DO: suspect structs are poor choice; probably want to subclass EntoliError




//**********************************************************************
// parse errors


class SyntaxError: Error, CustomStringConvertible {
    
    let description: String
    
    init(description: String = "Syntax error.") { // TO DO: also needs range + source
        self.description = description
    }
    
} // TO DO: how to store error info? (ideally, should be NSError-compatible without being dependent on it)

class EndOfCodeError: SyntaxError {}

class LeftOperandNotFoundError: SyntaxError {}

class MalformedNumericError: SyntaxError {}



//**********************************************************************


//struct BadValue: ErrorType {}
struct BadArgument: Error {
    let description: String
}


typealias MismatchedField = BadArgument // TO DO: actual type?




struct EvaluationError: Error {
    let description: String
}


enum NameSearchLocation {
    case inScope(Scope)
    case inValue(Value)
}


struct NameNotFoundError: Error {
    let name: Name
    let location: NameSearchLocation
    let description: String?
    
    init(name: Name, scope: Scope, description: String? = nil) {
        self.name = name
        self.location = .inScope(scope)
        self.description = description
    }
    init(name: Name, value: Value, description: String? = nil) {
        self.name = name
        self.location = .inValue(value)
        self.description = description
    }
    // TO DO: when generating detailed error messages, this should search scope chain/value's contents and generate a list of fuzzy matches as 'Did you mean...' suggestions
}



struct CastError: Error {
    let value: Value
    let type: Value.Type
}


struct CoercionError: Error {
    let value: Any // TO DO: what should this be? (e.g. enum of Value, Scalar, Primitive?)
    let coercion: Coercion?
    let description: String?
    // TO DO: should this also capture env:Scope?
    
    init(value: Any, coercion: Coercion? = nil, description: String? = nil) {
        self.value = value
        self.coercion = coercion
        self.description = description
    }
}



@noreturn func fatalNotYetImplemented(_ object: Any, _ methodName: String, _ message: String = "") {
    fatalError("\(object.dynamicType) does not yet implement \(methodName). \(message)")
}



struct NotSupportedError: Error {} // not an allowed operation


struct NullValueCoercionError: Error {
    let coercion: Coercion
}


struct ScopeError: Error { // locked slot, name not found, etc
    let description: String
}


struct ImplementationError: Error { // internal bug // TO DO: better just to use `assert`?
    let description: String
}


struct ProcedureError: Error {
    let name: String
    let arguments: [Value]
    let commandScope: Scope
    let procedureScope: Scope
    let originalError: Error
}


