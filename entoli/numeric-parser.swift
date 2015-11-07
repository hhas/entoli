//
//  numeric-parser.swift
//  entoli
//
//
//

import Darwin

// TO DO: once typespecs are implemented, these functions may become static methods on numeric typespecs

// TO DO: prefixes need to be configurable, e.g. to allow currency prefix symbols to be added

// note: text values could annotate themselves as NotANumber if scan fails, should that provide a benefit

// TO DO: will need to return range of numerical part, as parser will need to read unit suffix itself in case it contains symbol operators (currently, these will always end up in unit string, which is incorrect); it might even be simpler to split off suffix parsing entirely, e.g. as a separate func that can be called after this one (when parsing numeric strings in typespecs, which will want to read all the way to end); we'll also need to be careful when converting such values to code (although that applies to all unquoted text, which will need to check they don't contain any operators/reserved chars, etc and double-quote themselves if they do) 

// also need to think about non-space-delimited periods should be treated when surrounded on both sides by digits even when word isn't a number as such; should they be treated as decimal separators, and thus part of the text rather than an expression separator simply by association? e.g.:

// "0-123-7.34" -> (entoli.NumericValue.Integer(0, Optional("-123-7")), Range(0..<7))

// also need to think about how to modularize this numerical parsing (e.g. as composable/chainable funcs) to allow easy construction of more complex pattern matching (e.g. canonical dates and times)

// note: unlike Swift's exponent literal notation, this accepts decimal exponents (quite what use these would be is anyone's guess, but the math APIs do allow it) // TO DO: find out if there's a particular reason it's disallowed as a literal syntax in other languages, just in case they know something we don't

/**********************************************************************/


let gNumericDigits           = Set("01234567890".characters)
let gNumericSigns            = Set("+-".characters) // TO DO: should n-dashes also be accepted? (fuzzy matching)
let gNumericPrefixes         = Set("".characters) // TO DO: currency symbols ($, £, €, ¥, etc) need to be explicitly specified; should a default set be hardcoded here, or should they be entirely configurable? (note: for some tasks, users may wish to disable prefix/suffix units entirely)
let gDecimalSeparator        = Character(".") // currently this must be a member of `punctuation` (see above) for scanner to work correctly (ideally this'd be localizable, along with thousands separator, allowing [e.g.] `$1,000,000.00`, `€1.000.000,00`, etc)

let gExponentSeparators      = Set("Ee".characters)

let gCodepointSeparators     = Set("Uu".characters)
let gHexadecimalSeparators   = Set("Xx".characters)
let gHexadecimalDigits       = Set("01234567890AaBbCcDdEeFf".characters)


// TO DO: this needs moved to its own module, since typespecs will also want to use it to scan [previously untagged] TextValues when coercing them to numeric types for first time


/**********************************************************************/


typealias NumericUnit = String

// TO DO: probably want to break out numeric unit, as it makes using this enum needlessly complex; better to compose unit info as a struct or another enum (especially since unit suffixes need to be parsed differently when reading source code vs typespec-casting a text value)

enum NumericValue {
    // note: fractions could in principle also be allowed, e.g. represented in text form as `/` symbol operator is bounded by numbers, and stored here with numerator and denominator as Doubles
    case Integer(Int, NumericUnit?)
    case FloatingPoint(Double, NumericUnit?)
    case OutOfRange(String, NumericUnit?) // string is numeric value, sans units
    
    var intValue: Int? {
        switch self {
        case .Integer(let n, _):        return n
        case .FloatingPoint(let n, _):  return n % 1 == 0 ? Int(n) : nil
        case .OutOfRange:               return nil
        }
    }
    
    var doubleValue: Double {
        switch self {
        case .Integer(let n, _):        return Double(n)
        case .FloatingPoint(let n, _):  return n
        case .OutOfRange:               return Double.infinity
        }
    }
    
    var unit: NumericUnit? {
        switch self {
        case .Integer(_, let u):        return u
        case .FloatingPoint(_, let u):  return u
        case .OutOfRange(_, let u):     return u
        }
    }
}


/**********************************************************************/


private func toIntegerValue(code: ScriptChars, isNegative: Bool, units: NumericUnit? = nil, radix: Int = 10) -> NumericValue {
    let stringValue = String(code)
    if let number = Int(stringValue, radix: radix) {
        print(stringValue)
        return .Integer((isNegative ? -number : number), units)
    } else {
        return .OutOfRange(stringValue, units)
    }
}

private func toDoubleValue(code: ScriptChars, isNegative: Bool, units: NumericUnit? = nil) -> NumericValue {
    let stringValue = String(code)
    if let number = Double(stringValue) {
        return .FloatingPoint(isNegative ? -number : number, units)
    } else {
        return .OutOfRange(stringValue, units)
    }
}


/**********************************************************************/
// scan the given code from the specified index, to determine if it starts with a numeric word

// TO DO: should this always digest multiple sign (+/-) prefixes? (in code, they'd just be prefix ops otherwise, so it does no harm; not sure about text-to-number casts though - typespecs might want to pass an optional flag to disallow this 'sloppiness' and ensure no more than one sign char is allowed)

func toNumericValue(code: ScriptChars, start: ScriptIndex, allowPrefixes: Set<Character> = gNumericPrefixes,
                    allowExponent: Bool = true, allowUnitSuffix: Bool = true) -> (NumericValue, ScriptRange)? {
    // TO DO: ignore leading whitespace?
    let codeLength = code.endIndex
    var isNegative = false
    var prefixUnit: String? = nil
    var idx = start
    // 1. start by scanning for prefix chars; defaults are "+" and "-", but may be customized to include, for example, known currency symbols ("$", "£", "$", "¥", etc.)
    let firstChar = code[idx]
    let allowedPrefixChars = gNumericSigns.union(allowPrefixes)
    if idx < codeLength && allowedPrefixChars.contains(firstChar) {
        var prefixes = [Character]()
        var hasPrefixUnit = false
        while idx < codeLength { // scan over any prefix chars; TO DO: this should be more precise, rejecting if unit prefix chars appear both before and after numeric ops, e.g. `+$-1`, `£-€1`; also, what about, say, `±$1`? (suspect `±`, like currency symbols, is for consuming typespecs/procs to deal with)
            let char = code[idx]
            if !(allowedPrefixChars.contains(char)) { break }
            switch char {
            case "-": prefixes.append(" ");  isNegative = !isNegative // TO DO: manky hardcoding of raw chars; see gNumericSigns above (TBH, might want to split into gPositiveSign, gNegativeSign sets and use those if any additional chars are going to be added)
            case "+": prefixes.append(" ")
            default:  prefixes.append(char); hasPrefixUnit = true
            }
            idx = idx.successor()
        }
        if hasPrefixUnit { prefixUnit = String(prefixes) }
    }
    // 2. now check for a leading digit; if not found, this is not a numerical value
    let firstDigitIndex = idx
    let firstDigit = code[idx]
    if !(idx < codeLength && gNumericDigits.contains(firstDigit)) { return nil } // check for leading digit to confirm it's a numeric before proceeding
    // 3. if the leading digit is "0", check if it's a hexadecimal value; if it is, parse and return it...
    if prefixUnit == nil && firstDigit == "0" { // hex numbers cannot have unix prefix/suffix, and must always start with `0X`/`0x`
        var idx2 = idx.successor()
        if idx2 < codeLength {
            let isHex = gHexadecimalSeparators.contains(code[idx2])
            if isHex { // read remaining chars as hexadecimal // TO DO: `|| codepointSeparators.contains(code[idx2])`; need to decide how to represent; single UTF8 value, single UTF16 value, contiguous sequence of 1 or more UTF16 values
                idx2 = idx2.successor()
                let hexStart = idx2
                while gHexadecimalDigits.contains(code[idx2]) {
                    idx2 = idx2.successor()
                }
                if Lexer.reservedCharacters.contains(code[idx2]) { // found punctuation (i.e. end of word), so it's a unit-less integer
                    return (toIntegerValue(code[hexStart..<idx2], isNegative: isNegative, radix: 16), firstDigitIndex..<idx2)
                }
            }
        }
    }
    // 4. ...otherwise it's an integer or decimal number (base 10), so work out which
    repeat { // scan to end of contiguous digits (whole part)
        idx = idx.successor()
    } while idx < codeLength && gNumericDigits.contains(code[idx])
    var numericValue: NumericValue // temporary store for initial integer/decimal data (if exponent or unit suffix are subsequently found, this will be redone)
    if idx < codeLength && code[idx] == gDecimalSeparator { // is it a decimal? first, check for decimal separator (`.`)...
        idx = idx.successor()
        if idx < codeLength && gNumericDigits.contains(code[idx]) { // ...followed by a digit
            repeat { // it's a decimal number, so scan to end of contiguous digits (fractional part)
                idx = idx.successor()
            } while idx < codeLength && gNumericDigits.contains(code[idx])
            let range = firstDigitIndex..<idx
            numericValue = toDoubleValue(code[range], isNegative: isNegative, units: prefixUnit) // TO DO: using doubles to represent currency is not ideal; while it probably doesn't hurt to store Double representation here, coercion handlers and commands that deal specifically with currency values may want to use [e.g.] NSDecimal instead
            if idx == codeLength || Lexer.reservedCharacters.contains(code[idx]) { // found punctuation (i.e. end of word), so it's a suffix-less double
                return (numericValue, range)
            } // else there's more characters still to parse
        } else { // no digit after period (i.e. the period is an expression separator, not decimal separator), so it's a suffix-less integer
            let range = firstDigitIndex..<idx.predecessor() // ...so move back to period...
            return (toIntegerValue(code[range], isNegative: isNegative, units: prefixUnit), range) // ...and return it, as we're done
        }
    } else {
        let range = firstDigitIndex..<idx
        numericValue = toIntegerValue(code[range], isNegative: isNegative, units: prefixUnit)
        if idx == codeLength || Lexer.reservedCharacters.contains(code[idx]) { // found punctuation (i.e. end of word), so it's a suffix-less integer, and can be returned
            return (numericValue, range)
        }
    } // else it's an integer with an exponent and/or unit suffix
    // 5. there's still characters to read, so check if there's an exponent
    if allowExponent && idx < codeLength && gExponentSeparators.contains(code[idx]) { // is it an exponent? first we check for separator (`e`)...
        idx = idx.successor()
        // ...then try scanning for another positive/negative integer number after `e` separator
        if let (exponentValue, exponentRange) = toNumericValue(code, start: idx, allowPrefixes: gNumericSigns,
                                                               allowExponent: false, allowUnitSuffix: allowUnitSuffix && prefixUnit == nil) {
            // found an exponent, so extract it and unit suffix (if any)
            let range = start..<exponentRange.endIndex
            let exponent: Double
            let suffixUnit: NumericUnit?
            switch (exponentValue) {
            case .Integer(let e, let u):        exponent = pow(10.0, Double(e));    suffixUnit = u
            case .FloatingPoint(let e, let u):  exponent = pow(10.0, e);            suffixUnit = u
            case .OutOfRange(_, let u):         exponent = Double.infinity;         suffixUnit = u
            }
            if prefixUnit != nil && suffixUnit != nil { return nil } // can't have both prefix and suffix units
            let result: Double
            switch (numericValue) {
            case .Integer(let n, _):        result = Double(n) * exponent
            case .FloatingPoint(let n, _):  result = n * exponent
            case .OutOfRange(_, _):         result = Double.infinity
            }
            return ((result == Double.infinity ? .OutOfRange(String(code[range]), suffixUnit) : .FloatingPoint(result, suffixUnit)), range)
        } else {
            idx = idx.predecessor() // no digit after `e`, so move back to `e`
        }
    }
    // 6. lastly, any remaining characters will be read as an arbitrary unit type
    let suffixStart = idx
    while idx < codeLength && !Lexer.reservedCharacters.contains(code[idx]) {
        idx = idx.successor()
    }
    if suffixStart < idx {
        if prefixUnit != nil || !allowUnitSuffix { return nil } // check unit suffix is allowed, and return nil if not
        let suffixUnit = String(code[suffixStart..<idx])
        switch (numericValue) {
        case .Integer(let n, _):        numericValue = .Integer(n, suffixUnit)
        case .FloatingPoint(let n, _):  numericValue = .FloatingPoint(n, suffixUnit)
        case .OutOfRange(_, _):         numericValue = .OutOfRange(String(code[start..<idx]), suffixUnit)
        }
    }
    return (numericValue, start..<idx)
}
    
    