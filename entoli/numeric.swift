//
//  numeric.swift
//  entoli-run
//
//
//

//**********************************************************************
// unit support


struct NumericUnit { // TO DO: finish // eventually this will be object/struct (prob. not enum as it needs to be extensible) describing measurement system, unit name, unit aliases, unit conversions
    let type: String
    let isPrefix: Bool
}


// TO DO: suspect this should be a class; also need to think about how best to model unit categories (e.g. lengths, weights, areas, temperatures, etc); probably would make sense for each category to be a class [assuming they're hardcoded; if not, it'll require something table-driven], with each instance representing a single unit definition within that category (note: this also means that conversions need to be customizable at function level, since e.g. converting currency requires an external lookup to predefined table or live webservice); it's also possible that all unit values should be represented as Quantity instances that encapsulate both Scalar and Unit

struct NumericUnits {
    
    // TO DO: would be better to take unit definitions as array of NumericUnitDefinition(name,aliases,isPrefix,...) and build separate prefixes and suffixes arrays here, using normalized key strings
    
    // caution: while both prefixes and suffixes can be anything, not all can be written as unquoted text, which is the preferred form
    let prefixes: [String:NumericUnit] // keys must be dedicated symbols to avoid collisions with unquoted names, e.g. "$", "£", "€"
    let suffixes: [String:NumericUnit] // keys can be anything as long as they don't contain reserved characters, e.g. "kg", "g", "mg", "°C"
}

let gDefaultNumericUnits = NumericUnits(prefixes: [:], suffixes: [:])


//**********************************************************************
 // Numeric (anything that contains numeric information, from simple numbers to UTF8-encoded text literals)


enum Numeric {
    case Number(Scalar)
    case Quantity(Scalar, NumericUnit) // TO DO: probably also want an option to retain original/canonical value to minimize loss of precision when applying multiple conversions (e.g. `in`->`cm`->`pt`->`pica` will produce better results recalculating from original inches quantity each time)
    // TO DO: Vector? (TBH, that can be added if/when the need arises, e.g. as Array<Scalar>; as with dates, times, and other numerical data, the best representation will be determined by the implemented solution)
    case UTF8EncodedString(String) // decoded string
    case Invalid(String, NumericError) // String is word; NumericError indicates problem
    
    
    func literalRepresentation() -> String {
        switch self {
        case .Number(let n):
            return String(n)
        case .Quantity(let n, let u):
            return u.isPrefix ? "\(u)\(n)" : "\(n)\(u)" // TO DO: problem here is that `-` should probably appear before prefix
        case .UTF8EncodedString(let s):
            return s // TO DO: return UTF8 codepoints literal, e.g. "A\t∞" -> "0u41+9+E2889E"
        case .Invalid(let s, _):
            return s
        }
    }
}


enum NumericError { // TO DO: decide how best to implement (e.g. read word to completion, and include string representation and code range in error)
    case UnknownUnit // unknown unit suffix (note that unknown prefixes will result in word being read as unquoted name as there's no way to distinguish a non-digit char followed by digit chars from a valid name; it's contingent upon whoever defines unit prefixes not to use characters that are liable to appear at start of proc names, e.g. `a-z`)
    case InvalidCodePoint
    case Overflow(Any.Type)
    case NotNumber
    case Other
}



