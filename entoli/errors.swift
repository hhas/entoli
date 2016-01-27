//
//  errors.swift
//  entoli-run
//
//

// TO DO: suspect structs are poor choice; probably want to subclass EntoliError




//**********************************************************************
// parse errors


class SyntaxError: ErrorType, CustomStringConvertible {
    
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
struct BadArgument: ErrorType {
    let description: String
}


typealias MismatchedField = BadArgument // TO DO: actual type?




struct EvaluationError: ErrorType {
    let description: String
}


enum NameSearchLocation {
    case InScope(Scope)
    case InValue(Value)
}


struct NameNotFoundError: ErrorType {
    let name: Name
    let location: NameSearchLocation
    let description: String?
    
    init(name: Name, scope: Scope, description: String? = nil) {
        self.name = name
        self.location = .InScope(scope)
        self.description = description
    }
    init(name: Name, value: Value, description: String? = nil) {
        self.name = name
        self.location = .InValue(value)
        self.description = description
    }
    // TO DO: when generating detailed error messages, this should search scope chain/value's contents and generate a list of fuzzy matches as 'Did you mean...' suggestions
}



struct CastError: ErrorType {
    let value: Value
    let type: Value.Type
}


struct CoercionError: ErrorType {
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



@noreturn func fatalNotYetImplemented(object: Any, _ methodName: String, _ message: String = "") {
    fatalError("\(object.dynamicType) does not yet implement \(methodName). \(message)")
}



struct NotSupportedError: ErrorType {} // not an allowed operation


struct NullValueCoercionError: ErrorType {
    let coercion: Coercion
}


struct ScopeError: ErrorType { // locked slot, name not found, etc
    let description: String
}


struct ImplementationError: ErrorType { // internal bug // TO DO: better just to use `assert`?
    let description: String
}


struct ProcedureError: ErrorType {
    let name: String
    let arguments: [Value]
    let commandScope: Scope
    let procedureScope: Scope
    let originalError: ErrorType
}


