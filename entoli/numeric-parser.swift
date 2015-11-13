//
//  numeric-parser.swift
//  entoli
//
//
//

import Darwin


// TO DO: how to modularize this numerical parsing (e.g. as composable/chainable funcs) to allow easy construction of more complex pattern matching (e.g. canonical dates and times); also, how should complex structures such as dates be detected and matched? (ideally they'd be another form of suffix, being pattern matched on separator char, though whether such funcs should be passed for tail calling with accummulated data as arg or nested in composition remains to be decided) (note that while new function breakdown is better for reuse, it still isn't composable as there isn't a single standard API across them all: that would require e.g. passing a struct containing whatever info has been accummulated so far so that each subsequent func can decide what to do next)

// TO DO: both prefixes and suffixes should be pre-defined (avoids any confusion with operator chars appearing in suffix being read as part of that suffix); bonus: it should be possible to predefine them using a simple Dictionary<String,String> (aliases to canonical name; or to whatever eventually represents unit types); might need 2 dicts if doing separate prefix/suffix, given that prefixes are much more restrictive than suffixes (to avoid collisions with unquoted names, only dedicated symbols should be used, e.g. `$`, whereas suffixes could be almost anything, e.g. `USD`)



// TO DO: once typespecs are implemented, these functions may become static methods on numeric typespecs

// note: in typespecs, TextValues that aren't numbers could be annotated as Numeric.NotNumber the first time a numeric scan fails, eliminating need to rescan should that value be [unsuccessfully] coerced again

// TO DO: manipulating currency as doubles is not ideal; while it probably doesn't hurt to store numerical value as Scalar.FloatingPoint representation, coercion handlers and commands that deal specifically with currency values may want to ignore that and use [e.g.] NSDecimal instead


/**********************************************************************/


let gNumericDigits           = Set("01234567890".characters)
let gNumericSigns            = Set("+-".characters) // TO DO: should n-dashes also be accepted? (fuzzy matching)
let gDecimalSeparator        = Character(".") // currently this must be a member of `punctuation` (see above) for scanner to work correctly (ideally this'd be localizable, along with thousands separator, allowing [e.g.] `$1,000,000.00`, `€1.000.000,00`, etc)
let gExponentSeparator       = Set("Ee".characters)
let gCodePointPrefix         = Set("Uu".characters)
let gCodePointSeparator      = Set("+".characters)
let gHexadecimalPrefix       = Set("Xx".characters)
let gAllHexPrefixes          = gHexadecimalPrefix.union(gCodePointPrefix)
let gHexadecimalDigits       = Set("01234567890AaBbCcDdEeFf".characters)

let gHexadecimalMap: [Character:UInt8] = ["0":0, "1":1, "2":2, "3":3, "4":4,
                                          "5":5, "6":6, "7":7, "8":8, "9":9,
                                          "A":10, "B":11, "C":12, "D":13, "E":14, "F":15,
                                          "a":10, "b":11, "c":12, "d":13, "e":14, "f":15]


/**********************************************************************/


typealias NumericUnit = String // eventually this will be object/struct (prob. not enum as it needs to be extensible) describing measurement system, unit name, unit aliases, unit conversions



// TO DO:

struct NumericUnits {
    // caution: while both prefixes and suffixes can be anything, not all can be written as unquoted text, which is the preferred form
    let prefixes: [String:NumericUnit] // keys must be dedicated symbols to avoid collisions with unquoted names, e.g. "$", "£", "€"
    let suffixes: [String:NumericUnit] // keys can be anything as long as they don't contain reserved characters, e.g. "kg", "g", "mg", "°C"
}

let gDefaultNumericUnits = NumericUnits(prefixes: [:], suffixes: [:])


/**********************************************************************/


enum Scalar { // TO DO: what about including normalized representation, either here or in Numeric enum? (for display purposes entire representation wants to be in outermost; for decomposition it wants to be split into each)
    case Integer(Int)
    case FloatingPoint(Double)
    case Overflow(String) // string is original representation
    // TO DO: date, time, what else? (note: am inclined to hardcode these)
    case UTF8EncodedString(String) // sequence of one or more UTF8 codepoints, e.g. `0u41+9+E2889E` -> "A\t∞"
    case Malformed(String, NumericError) // invalid codepoint (i.e. out of range)
    
    // TO DO: could this implement reader?
    
    static func makeInt(code: ScriptChars, isNegative: Bool, radix: Int = 10) -> Scalar {
        if let number = Int(String(code), radix: radix) {
            return .Integer(isNegative ? -number : number)
        } else {
            return .Overflow(String(code))
        }
    }
    
    static func makeDouble(code: ScriptChars, isNegative: Bool) -> Scalar {
        if let number = Double(String(code)) {
            return .FloatingPoint(isNegative ? -number : number)
        } else {
            return .Overflow(String(code))
        }
    }
    
    var asInt: Int? {
        switch self {
        case .Integer(let n):       return n
        case .FloatingPoint(let n): return n % 1 == 0 ? Int(n) : nil
        default:                    return nil
        }
    }
    
    var asDouble: Double? {
        switch self {
        case .Integer(let n):       return Double(n)
        case .FloatingPoint(let n): return n
        case .Overflow:             return Double.infinity // TO DO: should this return nil, same as `asInt`?
        default:                    return nil
        }
    }
    
    // what about other representations? e.g. stringValue might convert CodePoint to String, but might also return literal representation of int/double too
}

typealias Unit = String


enum Numeric {
    case Number(Scalar)
    case Text(String) // unwrap Scalar.UTF8EncodedString
    case Quantity(Unit?, Scalar, Unit?) // note: one or both Unit values will be non-nil; e.g. `$` prefix, `kg` suffix; note that `(`, `)` are valid prefix and suffix (negation) in accountancy representation
    // TO DO: Vector? (TBH, that can be added if/when the need arises, e.g. as Array<Scalar>; as with dates, times, and other numerical data, the best representation will be determined by the implemented solution)
    case Malformed(String, NumericError) // String is word; NumericError indicates problem
}

enum NumericError { // TO DO: decide how best to implement (e.g. read word to completion, and include string representation and code range in error)
    case UnknownUnit // unknown unit suffix (note that unknown prefixes will result in word being read as unquoted name as there's no way to distinguish a non-digit char followed by digit chars from a valid name; it's contingent upon whoever defines unit prefixes not to use characters that are liable to appear at start of proc names, e.g. `a-z`)
    case InvalidCodePoint
    case Other
}



/**********************************************************************/
// scan the given code from the specified index, to determine if it starts with a numeric word
 
 /*
 Notes:
 
 - read funcs should be called in the following order:
 
     readNumberSign
     readUnitPrefix
     readHexadecimalNumber
     readDecimalNumber
     readUnitSuffix
     
 - unlike unit prefix/suffix recognition (which is extensible and table driven) hexadecimal, codepoint, and exponent recognition is hardcoded and always takes priority over unit suffixes
 */
 
 

// TO DO: should this always digest multiple sign (+/-) prefixes? (in code, they'd just be prefix ops otherwise, so it does no harm; not sure about text-to-number casts though - typespecs might want to pass an optional flag to disallow this 'sloppiness' and ensure no more than one sign char is allowed; typespecs should also wrap this call in a function that ignores leading+trailing whitespace, and fails if the input is not fully consumed)

// TO DO: also return normalized representation (either as string or as struct of component parts: prefixSign, prefixUnit, number, etc)? this'd allow pretty printer to tidy up excess signs and put signs and prefix units into consistent order

// TO DO: this needs to differentiate between non-number and malformed number (i.e. basic numeric syntax is a hardcoded special case, so any word that starts with a recognized digit but then violates the rules, e.g. by having both prefix and suffix units, or by having too many decimal points, or too many exponents, etc. should be flagged as malformed so that it cannot be subsequently read as an unquoted name or operator)



func readNumberSign(code: ScriptChars, var startIndex idx: ScriptIndex, var isNegative: Bool = false) -> (isNegative: Bool, endIndex: ScriptIndex) {
    // startIndex should be first char to scan; if not `+` or `-`, isNegative is unchanged and endIndex = startIndex
    while idx < code.endIndex {
        switch code[idx] {
        case "+": ()
        case "-": isNegative = !isNegative
        default: return (isNegative, idx)
        }
        idx = idx.successor()
    }
    return (isNegative, idx)
}



private struct CodePointReader: GeneratorType { // used by readHexadecimalNumber()
    typealias Element = UInt8
    private var subCodePoints: [UInt8]
    mutating func next() -> Element? {
        return self.subCodePoints == [] ? nil : self.subCodePoints.removeFirst() * 16 + self.subCodePoints.removeFirst()
    }
}





func readHexadecimalNumber(code: ScriptChars, startIndex: ScriptIndex, isNegative: Bool = false) -> (Scalar, ScriptIndex)? { // startIndex is for the entire hex literal (e.g. `0x0000`) minus any `+`/`-` prefixes (signs should already have been read by readNumberSign() and the result passed here as isNegative argument)
    // note: as with readNumber, this function is opportunistic and can be safely called to determine if a literal is a hex value and read and return it if it is, or return nil or .Integer(0) if not
    if code[startIndex] != "0" { return nil } // hex numbers and codepoints must always have `0X`/`0U` prefix (or `0x`/`0u`, since entoli code is case-insensitive)
    let codeLength = code.endIndex
    let typeCodeIndex = startIndex.successor()
    if typeCodeIndex == codeLength || !gAllHexPrefixes.contains(code[typeCodeIndex]) { return nil } // check for second char in prefix
    let firstHexDigitIndex = typeCodeIndex.successor()
    if firstHexDigitIndex == code.endIndex || !gHexadecimalDigits.contains(code[firstHexDigitIndex]) { // check prefix is followed by a hex digit
        return (Scalar.Integer(0), typeCodeIndex) // otherwise it's just a `0` with an `X`/`U` suffix
    }
    var chars = String.CharacterView()
    if gHexadecimalPrefix.contains(code[typeCodeIndex]) { // read remaining chars as [signed] hexadecimal integer between Int.min and Int.max
        var idx = firstHexDigitIndex
        if isNegative { chars.append("-") } // re-add sign
        while idx < code.endIndex && gHexadecimalDigits.contains(code[idx]) {
            chars.append(code[idx])
            idx = idx.successor()
        }
        if let value = Int(String(chars), radix: 16) {
            return (Scalar.Integer(value), idx)
        } else {
            return (Scalar.Overflow(String(code[firstHexDigitIndex..<idx])), idx)
        }
    } else { // read remaining chars as one or more hex-encoded UTF8 codepoints
        if isNegative { return nil } // TO DO: how best to deal with `-` prefix? e.g. `-0u30` isn't really valid; for now, the word will be read as `-0` integer followed by [presumably unknown] unit suffix `u30`
        var idx = typeCodeIndex
        // we don't know if there's an even or odd no. of digits until after they've all been read, so read each char as 4-bit int...
        var subCodePoints = [UInt8]()
        repeat {
            var tmp = [UInt8]()
            idx = idx.successor()
            while idx < code.endIndex && gHexadecimalDigits.contains(code[idx]) { // each sequence of contiguous hex digits represents a single codepoint
                tmp.append(gHexadecimalMap[code[idx]]!)
                idx = idx.successor()
            }
            if tmp.count % 2 != 0 { tmp.insert(0, atIndex: 0) } // ...then pad as needed to ensure an even number of items when done...
            subCodePoints.appendContentsOf(tmp) // ...and append to collection
        } while idx < code.endIndex && gCodePointSeparator.contains(code[idx]) // multiple codepoints are written as hex digit sequences separated by `+` chars (note: UTF16/UTF32 codepoints don't need explicit separators as they're fixed length, but UTF16 has endian nastiness and UTF32 is very verbose, and UTF8 is lingua franca)
        var decoder = UTF8()
        var codePoints = CodePointReader(subCodePoints: subCodePoints)
        var result = String.UnicodeScalarView()
        var isChar = true
        while isChar {
            switch decoder.decode(&codePoints) {
            case .Result(let c):
                result.append(c)
            case .EmptyInput:
                isChar = false
            case .Error:
                return (.Malformed(String(code[startIndex..<idx]), .InvalidCodePoint), idx)
            }
        }
        return (.UTF8EncodedString(String(result)), idx)
    }
}


// TO DO: parameterize thousands and decimal separators, e.g. for converting localized number values to canonical form

func readDecimalNumber(code: ScriptChars, startIndex: ScriptIndex, allowExponent: Bool = true, var isNegative: Bool = false) -> (value: Scalar, endIndex: ScriptIndex)? { // TO DO: return `nil` or .NotNumber value on non-match?
    // read an integer or double, with or without sign and/or exponent
    // note: this function can be called on any string to determine whether or not it starts with a valid number and consume and return it, along with an updated cursor index, if it is
    var idx = startIndex
    let codeLength = code.endIndex
    // read `+`/`-` sign, if any (multiple signs are automatically collapsed); note that this only detects signs that haven't already been read by an earlier read func, e.g. `$-100`
    (isNegative, idx) = readNumberSign(code, startIndex: idx, isNegative: isNegative)
    let firstDigitIndex = idx
    let firstDigit = code[firstDigitIndex]
    if idx == codeLength || !gNumericDigits.contains(firstDigit) { return nil } // check for leading digit to confirm it's a numeric before proceeding
    // proceed to read as int/double
    var scalar: Scalar // temporary store for initial integer/decimal data (if exponent or unit suffix are subsequently found, this will be redone)
    repeat { // scan to end of contiguous digits (whole part)
        idx = idx.successor()
    } while idx < codeLength && gNumericDigits.contains(code[idx])
    // now check 
    if idx < codeLength && code[idx] == gDecimalSeparator { // is it a decimal? first, check for decimal separator (`.`)...
        idx = idx.successor()
        if idx < codeLength && gNumericDigits.contains(code[idx]) { // ...followed by a digit
            repeat { // it's a decimal number, so scan to end of contiguous digits (fractional part)
                idx = idx.successor()
            } while idx < codeLength && gNumericDigits.contains(code[idx])
            scalar = Scalar.makeDouble(code[firstDigitIndex..<idx], isNegative: isNegative)
        } else { // no digit after period (i.e. the period is an expression separator, not decimal separator), so it's a suffix-less integer
            return (Scalar.makeInt(code[firstDigitIndex..<idx.predecessor()], isNegative: isNegative), idx) // ...and return it, as we're done
        }
    } else {
        scalar = Scalar.makeInt(code[firstDigitIndex..<idx], isNegative: isNegative)
    } // else it's an integer/double with an exponent

    if idx < codeLength && allowExponent && gExponentSeparator.contains(code[idx]) { // check for possible exponent (if allowed)
        // ...then try scanning for another positive/negative integer number after `e` separator
        // note: unlike Swift's exponent literal notation, which requires an integer exponent, this also accepts decimal exponents (quite what use fractional exponents would be is anyone's guess, but the standard math `pow()` takes exponent arg as double so it clearly allows it) // TO DO: find out if there's a particular reason fractional exponents are disallowed in literal syntax in other languages, just in case they know something we don't
        if let (exponentScalar, endIndex) = readDecimalNumber(code, startIndex: idx.successor(), allowExponent: false) {
            idx = endIndex
            var exponent: Double = 0
            var result: Double = 0
            var error: NumericError? = nil
            switch exponentScalar {
            case .Integer(let e):        exponent = pow(10.0, Double(e))
            case .FloatingPoint(let e):  exponent = pow(10.0, e)
            case .Overflow:              exponent = Double.infinity
            case .Malformed(_, let e):   error = e
            default:                     error = .Other // (in practice this never happens as .UTF8EncodedString is only returned by readHexadecimalNumber)
            }
            if error == nil {
                switch scalar {
                case .Integer(let n):        result = Double(n) * exponent
                case .FloatingPoint(let n):  result = n * exponent
                case .Overflow:              result = Double.infinity
                case .Malformed(_, let e):   error = e
                default:                     error = .Other // ditto
                }
            }
            if error == nil {
                scalar = result == Double.infinity ? .Overflow(String(code[startIndex..<endIndex])) : .FloatingPoint(result)
            }else {
                scalar = .Malformed(String(code[startIndex..<endIndex]), error!)
            }
        }
    }
    return (scalar, idx)
}




// composite readers


func isNumericWord(code: ScriptChars, startIndex: ScriptIndex, numericUnits: NumericUnits = gDefaultNumericUnits) -> Bool {
    // TO DO: this only needs to do a quick partial scan, sufficient to determine there's a digit after sign and/or unit prefix
    return readNumericWord(code, startIndex: startIndex, numericUnits: numericUnits) != nil
}



func readNumericWord(code: ScriptChars, startIndex: ScriptIndex, numericUnits: NumericUnits = gDefaultNumericUnits) -> (value: Numeric, endIndex: ScriptIndex)? {
    // note: this does *not* read to end of word as the rest of word may include symbol operators, e.g. `2.5cm*10` which need to be processed separately; instead, it reads to end of number/unit suffix and returns the index of the next char; the lexer should then continue to read, and report an "unknown suffix/malformed numeric" error if it doesn't find either a reserved char or a valid operator; in the case of a string->number typespec, it should scan to end of string, checking that only whitespace (which can be safely ignored) remains
    var (isNegative, idx) = readNumberSign(code, startIndex: startIndex)
    // TO DO: readUnit using numericUnits.prefixes
    let scalar: Scalar
    if let (result, endIndex) = readHexadecimalNumber(code, startIndex: startIndex, isNegative: isNegative)
        ?? readDecimalNumber(code, startIndex: startIndex, isNegative: isNegative) {
            scalar = result
            idx = endIndex
    } else {
        return nil
    }
    // TO DO: readUnit using numericUnits.suffixes
    let numeric: Numeric
    switch scalar {
    case .UTF8EncodedString(let s): numeric = .Text(s) // TO DO: error if units or isNegative
    case .Malformed(_, let e):      numeric = .Malformed(String(code[startIndex..<idx]), e)
    default:                        numeric = .Number(scalar)
    }
    return (numeric, idx)
}





    