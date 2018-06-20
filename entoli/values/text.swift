//
//  values/text.swift
//  entoli
//
//

import Foundation


//**********************************************************************
//


class Text: Value { // TO DO: how to annotate with numerics, units, dates, etc?
    let string: String
    
    
    init(_ string: String) {
        self.string = string
    }
    
    // literal representations // TO DO: entoli literal needs to escape any `"` chars by doubling them
    
    override var description: String { return "\"\(self.string.replacingOccurrences(of: "\"", with: "\"\""))\"" }
    override var debugDescription: String { return "\(type(of: self))(\(self.string.debugDescription))" } // TO DO: this isn't ideal as tab chars don't appear as `\t`; need to check if there are any other chars that don't display as they would be written in a Swift string literal
    
    
    // conversion // TO DO: these should self.annotate
    
    func toNumeric(_ numericUnits: NumericUnits = gDefaultNumericUnits) throws -> Numeric {
        // TO DO: first check for numeric annotation (which still needs to be implemented, natch)
        let code = self.string
        let startIndex = skipWhiteSpace(code, startIndex: code.startIndex)
        guard let (numeric, idx) = readNumericWord(code, startIndex: startIndex, numericUnits: numericUnits) else {
            throw ConstraintError(value: self, description: "Not a numeric value.")
        }
        if skipWhiteSpace(code, startIndex: idx) != code.endIndex { // found a numeric, but it was followed by more characters
            throw ConstraintError(value: self, description: "Not a numeric value (unknown characters at end).")
        }
        return numeric
    }
    
    func toScalar() throws -> Scalar {
        let numeric = try self.toNumeric()
        switch numeric {
        case .number(let scalar):
            return scalar
        default:
            throw ConstraintError(value: self, description: "Not a number: \(numeric)")
        }
    }
    
    // expansion
    
    override func _expandAsAny_(_ env: Scope) throws -> Value { return self }
    override func _expandAsText_(_ env: Scope) throws -> Text { return self }
    
    
    static func ==(lhs: Text, rhs: Text) -> Bool { // TO DO: all Values should be Equatable
        return lhs.string.lowercased() == rhs.string.lowercased() // TO DO: instead of normalizing each time, call Text.normalizedString, and let Text cache lowercase string for reuse
    }
}


let gEmptyText = Text("")

