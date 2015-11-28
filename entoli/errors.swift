//
//  errors.swift
//  entoli-run
//
//

// TO DO: suspect structs are poor choice; probably want to subclass EntoliError




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




struct CoercionError: ErrorType {
    let value: Any // TO DO: what should this be? (e.g. enum of Value, Scalar, Primitive?)
    let coercion: Coercion?
    let description: String?
    
    init(value: Any, coercion: Coercion? = nil, description: String? = nil) {
        self.value = value
        self.coercion = coercion
        self.description = description
    }
}

struct NotImplementedError: ErrorType {}


struct NotSupportedError: ErrorType {}


struct NullCoercionError: ErrorType {}


struct ScopeError: ErrorType { // locked slot, name not found, etc
    let description: String
}


struct ImplementationError: ErrorType {
    let description: String
}