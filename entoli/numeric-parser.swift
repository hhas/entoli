//
//  numeric-parser.swift
//  entoli
//
//
//

import Darwin // pow()


// TO DO: what about UInt? how important to support that too? it's possible entoli integers could support UInt.min..<UInt.max as maximum integer range by storing negation as a flag, though that could make math significantly more painful)
// TO DO: Int is 32-bit on non-64-bit systems, and [U]Int64 [presumably] isn't available, so need to rejig Scalar<->UInt[32/64] code to take that into account; also need to give some thought as to how to ensure 32-bit compatibility throughout (annoyingly, Swift stdlib doesn't define a standard generics-friendly protocol for instantiating all Integer and FloatingPoint types, so each needs to be implemented separately just to keep type system happy)


// TO DO: how to modularize this numerical parsing (e.g. as composable/chainable funcs) to allow easy construction of more complex pattern matching (e.g. canonical dates and times); also, how should complex structures such as dates be detected and matched? (ideally they'd be another form of suffix, being pattern matched on separator char, though whether such funcs should be passed for tail calling with accummulated data as arg or nested in composition remains to be decided) (note that while new function breakdown is better for reuse, it still isn't composable as there isn't a single standard API across them all: that would require e.g. passing a struct containing whatever info has been accummulated so far so that each subsequent func can decide what to do next)

// TO DO: both prefixes and suffixes should be pre-defined (avoids any confusion with operator chars appearing in suffix being read as part of that suffix); bonus: it should be possible to predefine them using a simple Dictionary<String,String> (aliases to canonical name; or to whatever eventually represents unit types); might need 2 dicts if doing separate prefix/suffix, given that prefixes are much more restrictive than suffixes (to avoid collisions with unquoted names, only dedicated symbols should be used, e.g. `$`, whereas suffixes could be almost anything, e.g. `USD`)



// TO DO: once typespecs are implemented, these functions may become static methods on numeric typespecs

// note: in typespecs, Texts that aren't numbers could be annotated as Numeric.NotNumber the first time a numeric scan fails, eliminating need to rescan should that value be [unsuccessfully] coerced again

// TO DO: manipulating currency as doubles is not ideal; while it probably doesn't hurt to store numerical value as Scalar.FloatingPoint representation, coercion handlers and commands that deal specifically with currency values may want to ignore that and use [e.g.] NSDecimal instead


// Q. regarding chainable parsefuncs, these would need to accept and return (ScriptChars,ScriptIndex,Result), where Result is a struct/class/whatever; the challenge is how to describe that Result, e.g. using multiple protocols, each specific to a particular parsefunc, that allow parsefunc to transform existing result and or attach additional info to it without knowing any details of how it's actually implemented (Q. how to type it, e.g. generic funcs? thus Result's type going in and out would be <ReturnType where ReturnType: SpecificProtocol>)


// TO DO: how practical to support localization in code? (it'd require an explicit locale declaration at top of file to eliminate guesswork, and supported locales would likely have to be hardcoded as comma/period overloading can impact lexer/parser behavior)


// TO DO: if canonical time has `HH:MM[:SS][TZ]` format (which it pretty much needs to), parser will need to take care not to confuse that for pair(s); as with `YYYY-MM-DD` dates, it's a case of numeric parsefunc looking for the longest match, and yielding to standard parser only if that exact form isn't found (thus even a minor, fairly natural change such as parenthesizing or spacing disrupts the 'special' form; BTW, formatter will need to take this into account too, and ensure it never outputs ambiguous forms, thus all of these numeric forms will need to be fixed and hardcoded in core, even if they're implemented internally as pluggable parsefuncs)


//**********************************************************************
// TO DO: the following are shared by both numeric-parser and lexer; where & how best to define? (e.g. on Script[Value]?)


typealias ScriptChars = String.CharacterView
typealias ScriptIndex = ScriptChars.Index
typealias ScriptRange = Range<ScriptIndex> // position of this token within the original source code (note: this may be different size to Token.value due to white space and operator name normalization)



//**********************************************************************


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


//**********************************************************************
// parsefuncs (these scan the given code from the specified index, to determine if it starts with a numeric word)
 
 /*
 Notes:
 
 - read funcs should be called in the following order:
    
     readUTF8EncodedTextLiteral
 
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


// TO DO: gut instinct is to split out readUTF8EncodedTextLiteral as first func to call; we gain nothing really by overloading here (a separate method will duplicate the first few lines, but that's nothing overall and will avoid stinking up Scalar type with String stuff)




func readUTF8EncodedTextLiteral(code: ScriptChars, startIndex: ScriptIndex) -> (Numeric, ScriptIndex)? { // startIndex is for the entire literal (`0u...`)
    // note: as with readNumber, this function is opportunistic and can be safely called to determine if a literal is a hex value and read and return it if it is, or return nil if not
    
    if code[startIndex] != "0" { return nil }
    var idx = startIndex.successor()
    let codeLength = code.endIndex
    if idx == codeLength || !gCodePointPrefix.contains(code[idx]) { return nil }
    let idx2 = idx.successor()
    if idx2 == code.endIndex || !gHexadecimalDigits.contains(code[idx2]) { return nil } // check prefix is followed by a hex digit
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
            return (.Invalid(String(code[startIndex..<idx]), .InvalidCodePoint), idx)
        }
    }
    return (.UTF8EncodedString(String(result)), idx)
}




func readHexadecimalNumber(code: ScriptChars, startIndex: ScriptIndex, isNegative: Bool = false) -> (Scalar, ScriptIndex)? { // startIndex is for the entire hex literal (e.g. `0x0000`) minus any `+`/`-` prefixes (signs should already have been read by readNumberSign() and the result passed here as isNegative argument)
    // note: as with readNumber, this function is opportunistic and can be safely called to determine if a literal is a hex value and read and return it if it is, or return nil or .Integer(0) if not
    if code[startIndex] != "0" { return nil } // hex numbers and codepoints must always have `0X`/`0U` prefix (or `0x`/`0u`, since entoli code is case-insensitive)
    let codeLength = code.endIndex
    let typeCodeIndex = startIndex.successor()
    if typeCodeIndex == codeLength || !gAllHexPrefixes.contains(code[typeCodeIndex]) { return nil } // check for second char in prefix
    var idx = typeCodeIndex.successor()
    if idx == code.endIndex || !gHexadecimalDigits.contains(code[idx]) { return (Scalar.Integer(0), typeCodeIndex) } // check prefix is followed by a hex digit otherwise it's just a `0` with an `X`/`x` suffix
    var chars = String.CharacterView()
    while idx < code.endIndex && gHexadecimalDigits.contains(code[idx]) {
        chars.append(code[idx])
        idx = idx.successor()
    }
    return (try! Scalar(int: chars, isNegative: isNegative, radix: 16), idx) // note: parsefunc has already verified it's valid so this should never throw exception unless there's a bug
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
            scalar = try! Scalar(double: code[firstDigitIndex..<idx], isNegative: isNegative)
        } else { // no digit after period (i.e. the period is an expression separator, not decimal separator), so it's a suffix-less integer
            idx = idx.predecessor()
            return (try! Scalar(int: code[firstDigitIndex..<idx.predecessor()], isNegative: isNegative), idx) // ...and return it, as we're done
        }
    } else {
        scalar = try! Scalar(int: code[firstDigitIndex..<idx], isNegative: isNegative)
    } // else it's an integer/double with an exponent

    if idx < codeLength && allowExponent && gExponentSeparator.contains(code[idx]) { // check for possible exponent (if allowed)
        // ...then try scanning for another positive/negative integer number after `e` separator
        // note: unlike Swift's exponent literal notation, which requires an integer exponent, this also accepts decimal exponents (quite what use fractional exponents would be is anyone's guess, but the standard math `pow()` takes exponent arg as double so it clearly allows it) // TO DO: find out if there's a particular reason fractional exponents are disallowed in literal syntax in other languages, just in case they know something we don't
        if let (exponentScalar, endIndex) = readDecimalNumber(code, startIndex: idx.successor(), allowExponent: false) {
            idx = endIndex
            var exponent: Double = 0
            var result: Double = 0
            var hasOverflow: Bool = false
            switch exponentScalar {
            case .Integer(let e):        exponent = pow(10.0, Double(e))
            case .FloatingPoint(let e):  exponent = pow(10.0, e)
            case .Overflow:              hasOverflow = true
            }
            if !hasOverflow {
                switch scalar {
                case .Integer(let n):        result = Double(n) * exponent
                case .FloatingPoint(let n):  result = n * exponent
                case .Overflow:              hasOverflow = true
                }
            }
            scalar = hasOverflow ? .Overflow(String(code[startIndex..<endIndex]), Double.self)  : .FloatingPoint(result)
        }
    }
    return (scalar, idx)
}




// composite readers


func isNumericWord(code: ScriptChars, startIndex: ScriptIndex, numericUnits: NumericUnits = gDefaultNumericUnits) -> Bool {
    // TO DO: this only needs to do a quick partial scan, sufficient to determine there's a digit after sign and/or unit prefix
    return readNumericWord(code, startIndex: startIndex, numericUnits: numericUnits) != nil
}



// TO DO: pass optional flag indicating if value should be decimal number only, dec or hex, and/or UTF8-encoded text (TBH, would be better if this was all determined by chaining modular parsefuncs, allowing any combination of acceptable forms to be specified, including more complex forms such as dates and times)

// TO DO: if no match, return (.NotNumber, startIndex) instead of nil? (would provide a bit more consistency, and allows Text to be annotated as non-numeric for future reference)

func readNumericWord(code: ScriptChars, startIndex: ScriptIndex, numericUnits: NumericUnits = gDefaultNumericUnits) -> (value: Numeric, endIndex: ScriptIndex)? { // returns `nil` if not a number
    // note: this does not accept leading whitespace and ignores trailing whitespace
    // note: this does *not* read to end of word as the rest of word may include symbol operators, e.g. `2.5cm*10` which need to be processed separately; instead, it reads to end of number/unit suffix and returns the index of the next char; the lexer should then continue to read, and report an "unknown suffix/malformed numeric" error if it doesn't find either a reserved char or a valid operator; in the case of a string->number typespec, it should scan to end of string, checking that only whitespace (which can be safely ignored) remains
    if let result = readUTF8EncodedTextLiteral(code, startIndex: startIndex) { return result } // returns either .UTF8EncodedString (or .Invalid if malformed) or nil if no match was made
    var (isNegative, idx) = readNumberSign(code, startIndex: startIndex)
    // TO DO: readUnit using numericUnits.prefixes
    let scalar: Scalar
    if let (result, endIndex) = readHexadecimalNumber(code, startIndex: startIndex, isNegative: isNegative) ?? readDecimalNumber(code, startIndex: startIndex, isNegative: isNegative) {
        scalar = result
        idx = endIndex
    } else {
        return nil
    }
    // TO DO: readUnit using numericUnits.suffixes
    let numeric: Numeric
    switch scalar {
    case .Overflow(_, let t):   numeric = .Invalid(String(code[startIndex..<idx]), .Overflow(t))
    default:                    numeric = .Number(scalar)
    }
    return (numeric, idx)
}


// utility func

let gAnyWhiteSpace = " \t\n\r".characters

func skipWhiteSpace(code: ScriptChars, var startIndex idx: ScriptIndex) -> ScriptIndex { // scans chars, starting at startIndex, until it finds first non-whitespace char (or code.endIndex) and returns its index
    while idx < code.endIndex && gAnyWhiteSpace.contains(code[idx]) { idx = idx.successor() }
    return idx
}



