//
//  punctuation-lexer.swift
//  entoli
//
//
//  Note that the meanings assigned to (...), [...], and {...} syntaxes are determined by 1. use of standard math notation (i.e. parentheses = grouping), 2. requirement not to overload meaning (i.e. parentheses cannot also be used for labeled/unlabeled tuples [records]†), 3. visual similarity to Swift (i.e. brackets = ordered lists [Set], key-value lists [Dictionary], and unique lists [Set]), 4. visual similarity to AppleScript (i.e. curly braces = records [labeled/unlabeled tuples]).
//
//  † Using parens for tuples wouldn't work anyway as a Swift-like tuple syntax requires static type info to disambiguate its meaning (since Swift overloads parens to indicate grouping and argument/parameter lists as well). See also the pain of Python tuple syntax, where empty tuple is `()`, multi-item tuple can be `(1,2,…)` or `1,2,…` (since technically it's the commas, not the parens that denote a Python tuple), and single-item tuple _must_ be `(1,)` or `1,`, NOT `(1)` (which is a group). Using curly braces to denote argument lists may feel weird (at least to C* programmers, who are used to using them to denote blocks), but traditional C* `foo(...)` command syntax is really just a degenerate form of unary `foo ARG` syntax where ARG is always some tuple, so `foo {...}` is completely consistent with that (since entoli commands really do use unary `foo ARG` syntax). As an aide-memoire, [Obj]C programmers can think of entoli records as equivalent to C structs (both are product values whose literals are denoted by curly braces), not C block syntax; Swift programmers are SOOL as Swift's equivalent is labeled/unlabeled tuples, which use parens, not braces, so will just have to suck it up and get used to the difference.



//  TO DO: consider evolving lexer (and parser?) in same direction as numeric-parser.swift is starting to go, as functions that take accummulated results plus cursor position and return new accummulated results plus new cursor position; Q. given that lexer/parser also want to support incremental parsing, and ideally some degree of live AST editing, in order to support 'smart' editors, how would such an approach fit with that?


// note: an entoli script can contain three things: code, user docs (annotations) and developer docs (comments) // TO DO: in implementation terms, can comments safely be treated as a particular type of annotation, with different code delimiters but same the syntax rules and internal storage mechanism? Or do they need more flexibility/hygene, e.g. since they're often also used temporarily to block out broken/unfinished code during development and testing? (note: the same 3-prong approach - user+dev+trace info - should apply to error reporting)


// TO DO: how hard to change expression separator from period to comma, and use period as block terminator? This'd allow users to write `To PROCNAME {name1, name2}, do this, do that, do the other; else fib, fub, zub.` It might also help reduce confusion over when to use commas versus periods (not that the expression parser really cares as it already treats both as separators anyway). It also allows REPL to trigger auto-eval as soon as user types period after one or more exprs. And blocks should be fully composable, being exprs themselves, so will be perfectly happy being sub-separated this way. All of which allows the `do` operator to become more of a convenience, for pairing blocks to [e.g.] `if', `repeat`, etc. (Though still need to think how ops such as `else` and `catching` will interact.) One disadvantage is nesting, since `do...done` are much easier to balance - and see are balanced - than `do xxxx.`, plus they avoid ugly case of multiple continguous periods, which are painful to read when immediately adjoining or look unnatural and are easy to miss when split across multiple lines, and in both cases look like a typo (or ellispis), not a block terminator.



private let DEBUG = false


//**********************************************************************
// Note: unquoted text is greedily parsed for potential operator names using longest possible match (e.g. the symbol-based operator name `>=` will preferentially match over `>`; similarly, the word-based operator name `is not equal to` will preferentially match over `is not`, which in turn will preferentially match over `is`); PartialOperatorMatch structs are used to gather the intermediate results; each time a full match is arrived at that particular struct is preserved as the longest current full match before continuing; once the longest partial match fails, the longest full operator match is converted to an operator Token and the lexer backtracked to resume parsing from the point where that match ended.

 
private struct PartialOperatorMatch<T:Hashable> { // T must be Character or String // TO DO: don't think Swift's type system is smart enough to express `Character || String` unfortunately, so for now T is just constrained to Hashable for implementation's sake, but it'd be nice if this could be tightened in future
    let precedingWords: [Lexer.UnquotedWord] // any preceding unquoted words already matched to LH side of match (if the operator is successfully matched, these will be added to cache as UnquotedName token, followed by the Operator token)
    let currentPartialWord: Lexer.PartialWord? // when matching a symbol operator in-word, the word's preceding characters (if any); nil when matching phrase operators // TO DO: make this non-nil? // TO DO: add endIndex to tuple (avoids dodgy coupling in update func)
    let isLeftDelimited: Bool // is there an explicit delimiter on LH side of match? (if false, the operator definition's autoDelimit flag will be used)
    let startIndex: ScriptIndex // position in source code at which the whole match begins
    private(set) var endIndex: ScriptIndex // position in source code at which this partial/whole match ends
    private(set) var info: OperatorTable<T>.Part // struct describing the current char/word match along with lookup table for making the next char/word match
    
    mutating func update(_ endIndex: ScriptIndex, info: OperatorTable<T>.Part) { // called by updateOperatorMatches
        self.endIndex = endIndex
        self.info = info
    }
}


//**********************************************************************


class Lexer {
    
    // Punctuation tokens (these are hardcoded and non-overrideable)
    
    // quoted text/name literals
    static let quoteDelimiters: [Character:TokenType] = [
        "\"": .quotedText,
        "“": .quotedText,
        "”": .quotedText,
        "'": .quotedName,
        "‘": .quotedName,
        "’": .quotedName,
    ]
    
    // annotation literals
    static let annotationDelimiters: [Character:Int] = [
        "«": 1,
        "»": -1,
    ]
    
    // collection literal/expression group delimiters, and separators
    static let punctuation: [Character:TokenType] = [
        "[": .listLiteral,
        "]": .listLiteralEnd,
        "{": .recordLiteral,
        "}": .recordLiteralEnd,
        "(": .groupLiteral,
        ")": .groupLiteralEnd,
        ".": .sentenceSeparator, // also decimal sep in canonical numbers (or thousands sep in localized numbers?)
        ",": .clauseSeparator, // also thousands separator in canonical numbers? (or decimal sep in localized numbers?)
        ":": .pairSeparator,
        ";": .pipeSeparator,
        // TO DO: should "@" and "#" also be fixed tokens? (used in `@mentions` and `#hashtag`, which may appear in code as well as in annotations; the former to identify universal [persistent machine-wide URI-like] resources, the latter to tag words for search indexing)
        "\n": .lineBreak,
        "\r": .lineBreak, // TO DO: how best to normalize CR/CRLF/LF? (note that this gets extra tricky inside text literals; prob best always to use LF internally and normalize at IO only; if user specifically wants CR, they should use [e.g.] ` "..." & 0u000C & "..."`)
    ]
    
    static let nonBreakingWhiteSpace: Set<Character> = [" ", "\t"]
    
    static let reservedCharacters = Set(quoteDelimiters.keys).union(Set(annotationDelimiters.keys))
        .union(Set(punctuation.keys)).union(Set(nonBreakingWhiteSpace)) // note: VocabularyTokenizer should never overload or redefine these tokens; e.g. to read an unquoted decimal number, it should look for digit[s]+ExpressionSeparator+digit[s]
    
    //**********************************************************************
    // Initialization
    
    let code: ScriptChars
    var cursor: ScriptIndex
    let codeLength: ScriptIndex
    private let operatorsTable: Operators
    private let numericUnits: NumericUnits
    
    // TO DO: how would/should lexer tie into incremental parsing support? would the editor, given knowledge of the AST (which in turn lets it determine which token user is currently modifying), simply re-lex after each edit? also, bear in mind the need to lex as code is edited (which means lexer needs to be able to suspend and resume on input stream, which this implementation - which uses String, not stream - currently doesn't allow)
    // TO DO: if per-line parsing, option to indicate already inside quoted text/name [1] or annotation
    init(code: String, operatorsTable: Operators = StandardOperatorsTable, numericUnits: NumericUnits = gDefaultNumericUnits) {
        self.code = code
        self.cursor = self.code.startIndex
        self.codeLength = self.code.endIndex
        self.operatorsTable = operatorsTable
        self.numericUnits = numericUnits
    }
    
    
    //**********************************************************************
    // supporting methods used by readVocabulary
    
    private typealias PartialPhraseOperatorMatch = PartialOperatorMatch<Operators.Phrase.Element>
    private typealias PartialSymbolOperatorMatch = PartialOperatorMatch<Operators.Symbol.Element>
    private typealias OperatorFixity             = (prefix: Bool, infix: Bool)
    private typealias FullPhraseOperatorMatch    = (match: PartialPhraseOperatorMatch, fixity: OperatorFixity) // TO DO: rename PartialOperatorMatch to OperatorMatch and add `private(set) var fixity: OperatorFixity?` to that
    private typealias FullSymbolOperatorMatch    = (match: PartialSymbolOperatorMatch, fixity: OperatorFixity)
    
    typealias UnquotedWord               = (text: String, startIndex: ScriptIndex, endIndex: ScriptIndex)
    typealias PartialWord                = (chars: String, startIndex: ScriptIndex, endIndex: ScriptIndex)
    
    //
    
    private func joinWords(_ words: [UnquotedWord]) -> String {
        return words.map{$0.text}.joined(separator: " ")
    }
    
    private func wordsRange(_ words: [UnquotedWord]) -> ScriptRange {
        return words.first!.startIndex..<words.last!.endIndex
    }
    
    // determine if a matched char/word sequence is a valid operator name (i.e. it's self-delimiting or not bounded by additional chars/words)
    
    // TO DO: parameterize these: they shouldn't rely on cursor's current position but should instead take current match's endIndex as arg
    
    private func isEndOfPhrase(_ idx: ScriptIndex) -> Bool { // used by isValidOperatorName() to determine if there are any more words in current unquoted words sequence (note: this does not test if subsequent word is a left-delimited operator, which also needs to be done if this test returns false)
        var idx = idx
        repeat { // TO DO: BUG?????? check if cursor is on last char of word or already on char after it; if the latter, then use `while...{}` loop instead
            idx = self.code.index(after: idx)
        } while idx < self.codeLength && Lexer.nonBreakingWhiteSpace.contains(self.code[idx])
        return idx == self.codeLength || Lexer.reservedCharacters.contains(self.code[idx])
                                      || isNumericWord(self.code, startIndex: idx, numericUnits: self.numericUnits)
    }
    
    private func isEndOfWord(_ idx: ScriptIndex) -> Bool { // idx is the char after the operator char being tested; as above, returns false if there are still characters to be consumed, requiring an additional test to disambiguate adjoining operators
       //print("isEndOfWord for `\(self.code[idx])`")
        return idx == self.codeLength || Lexer.reservedCharacters.contains(self.code[idx])
    }
    
    private var isFollowedByAutoLeftDelimitedOperator: Bool { // TO DO: this logic should prob move into isRightDelimited func, simplifying API; might also need to be generic, depending how it's implemented (alternatively, it might recursively tokenize ahead, which is direction in which lexer is evolving anyway)
        // Consider two adjoining operators, `AB`. If A is not right-auto-delimited, the lexer will want to consume B as well, returning a name, 'AB'. However, if B is left-auto-delimited, then it will force itself to be an operator, which in turn makes A explicitly left-delimited, allowing it to be an operator as well. One option might be to add A to the cache as a MaybeOperator, then continue reading tokens until something that isn't a MaybeOperator is reached, at which point the lexer can work backwards, transforming MaybeOperators into Operators or else removing them from the cache until it's back where it's started and can replace MaybeOperator-A with Name-AB. Alternatively, methods such as isEndOfPhrase might be rejigged to take index as argument, allowing updateOperatorMatches to be called recursively in pure lookahead without adding any tokens to the cache (though its returned tokens might be collected and added at the end should they all turn out to be operators after all). While this defect isn't an immediate blocker as there are currently very few operator combinations where this ambiguity will arise (e.g. `x+-y`), it will need to be addressed at some point.
        
        
        print("CAUTION: Auto-delimiting doesn't yet work for adjoining operators; code may parse incorrectly as a result. \(String(self.code.suffix(from:self.cursor)).debugDescription): \(self.currentToken)")
        return false // TO DO: fix; check if next char[s]/word[s] can form an operator; if they do, and they're auto-delimiting, then return true
        
    }
    
    private func isPrefixOperatorName(_ operatorDefinition: PrefixOperatorDefinition?,
                                      isLeftDelimited: Bool, isRightDelimited: (ScriptIndex)->Bool) -> Bool { // check if matched word[s] are 1. a complete operator name, and 2. delimited as per operator definition's requirements
        if let opDef = operatorDefinition { // non-nil if the operator name has been fully matched
            // note: if the matched "operator" is NOT left-self-delimiting, it must be first in word sequence; conversely, if it is NOT right-self-delimiting, it must be last. If these conditions are not met then it is not an operator, just normal word[s] within a longer phrase. e.g. The `to` prefix operator is not left-self-delimiting, so `to foo` is a valid `to` op with 'foo' as its RH operand, but `go to` is just an ordinary name (i.e. the 'to' is not special as it is not the first word in the expression).
            return (isLeftDelimited || opDef.name.autoDelimit.left)
                && (opDef.name.autoDelimit.right || isRightDelimited(self.cursor) || self.isFollowedByAutoLeftDelimitedOperator)
        }
        return false
    }
    
    private func isInfixOperatorName(_ operatorDefinition: InfixOperatorDefinition?,
                                     isLeftDelimited: Bool, isRightDelimited: (ScriptIndex)->Bool) -> Bool { // check if matched word[s] are 1. a complete operator name, and 2. delimited as per operator definition's requirements
        if let opDef = operatorDefinition { // non-nil if the operator name has been fully matched
            // note: if the matched "operator" is NOT left-self-delimiting, it must be first in word sequence; conversely, if it is NOT right-self-delimiting, it must be last. If these conditions are not met then it is not an operator, just normal word[s] within a longer phrase. e.g. The `to` prefix operator is not left-self-delimiting, so `to foo` is a valid `to` op with 'foo' as its RH operand, but `go to` is just an ordinary name (i.e. the 'to' is not special as it is not the first word in the expression).
            return (isLeftDelimited || opDef.name.autoDelimit.left)
                && (opDef.name.autoDelimit.right || isRightDelimited(self.cursor) || self.isFollowedByAutoLeftDelimitedOperator)
        }
        return false
    }

    
    private func updateOperatorMatches<T:Hashable>(_ operatorsTable: OperatorTable<T>,
                                                   partialMatches: inout [PartialOperatorMatch<T>],
                                                   fullMatch: inout (match: PartialOperatorMatch<T>, fixity: OperatorFixity)?, // once first full match is made, it is cached here, and no later operators are matched
                                                   value: T, startIndex: ScriptIndex, endIndex: ScriptIndex, // startIndex..<endIndex is range of current word (if matching Phrase) or char (if matching Symbol)
                                                   precedingWords: [UnquotedWord], currentPartialWord: PartialWord?,
                                                   isLeftDelimited: Bool, isRightDelimited: (ScriptIndex)->Bool) -> Bool {
        // check for the start of a new operator (note: this only needs done until the first full match is made)
        if fullMatch == nil {
            if let matchInfo = operatorsTable.definitionsByPart[value] { // matched the first char/word of a possible operator
                if DEBUG {print("Got a new operator match:  `\(value)`, isLeftDelimited=\(isLeftDelimited)")}
                partialMatches.append(PartialOperatorMatch<T>(precedingWords: precedingWords, currentPartialWord: currentPartialWord,
                                                              isLeftDelimited: isLeftDelimited, startIndex: startIndex, endIndex: endIndex, info: matchInfo))
            }
        }
        // check each current partial operator match (partial matches are ordered from earliest to latest; goal is to find the earliest, longest full match); if a full valid operator name is matched, store it in fullMatch and concentrate solely on finding longest version of that match; otherwise update or discard each partial match depending on whether or not it continues to match on current word
        var partialMatchIndex = 0
        while partialMatchIndex < partialMatches.count {
            let match = partialMatches[partialMatchIndex]
            if match.endIndex == endIndex { // TO DO: check this; supposed to prevent new match added above from being double-matched
                if DEBUG {print("...skipping new match `\(String(describing: match.info.name))`")}
            } else {
                if DEBUG {print("...updating existing match `\(String(describing: match.info.name))` with `\(value)` at \((match.endIndex, endIndex))")}
                if let nextMatchInfo = match.info.nextWords[value] {
                    partialMatches[partialMatchIndex].update(endIndex, info: nextMatchInfo)
                    if DEBUG {print("... and matched `\(value)` to it too: \(String(describing: partialMatches[partialMatchIndex].info.name))")}
                } else {
                    if DEBUG {print("...and failed to make a match on `\(value)`, so discarding partial.")}
                    partialMatches.remove(at: partialMatchIndex)
                    if partialMatches.count == 0 { return fullMatch != nil }
                    continue
                }
            }
            do {
                let match = partialMatches[partialMatchIndex]
                // check if we've found a complete, valid (i.e. correctly delimited) operator name
                let isPrefix = self.isPrefixOperatorName(match.info.prefixDefinition, isLeftDelimited: isLeftDelimited, isRightDelimited: isRightDelimited)
                let isInfix = self.isInfixOperatorName(match.info.infixDefinition, isLeftDelimited: isLeftDelimited, isRightDelimited: isRightDelimited)
                if isPrefix || isInfix { // found a full, correctly delimited operator name
                    if DEBUG {print("FOUND A FULL MATCH: `\(String(describing: match.info.name))`")}
                    fullMatch = (match, (isPrefix, isInfix))
                    partialMatches = [match]
                    if match.info.isLongest { return true }
                }
                partialMatchIndex += 1
            }
        }
        return false
    }
    
    private func tokensForMatchedOperator<T>(_ match: PartialOperatorMatch<T>, fixity: OperatorFixity) -> [Token] {
        // once the best operator match is made, call this method to obtain .UnquotedName token for preceding words (if any) and the finished .Operator token
        var nameToken: Token? = nil
        if match.precedingWords.count > 0 || (match.currentPartialWord?.chars.count ?? 0) > 0 {
            var words = match.precedingWords
            if let partialWord = match.currentPartialWord {
                if partialWord.chars.count > 0 {
                    words.append((String(match.currentPartialWord!.chars), partialWord.startIndex, partialWord.endIndex)) // TO DO: confirm these indexes are right
                }
            }
            nameToken = Token(type: .unquotedName, value: self.joinWords(words), range: self.wordsRange(words))
        }
        if DEBUG {print("FULLY MATCHED \(T.self) OPERATOR: <\(String(describing: match.info.name))>     range=\(match.startIndex..<match.endIndex)")}
        let operatorToken = Token(type: .operatorName, value: match.info.name!, range: match.startIndex..<match.endIndex,
                                                   operatorDefinitions: (fixity.prefix ? match.info.prefixDefinition : nil,
                                                                         fixity.infix  ? match.info.infixDefinition  : nil))
        return nameToken == nil ? [operatorToken] : [nameToken!, operatorToken]
    }
    
    
    //**********************************************************************
    // Vocabulary reader (used by main tokenizer function to process unquoted word sequences)
    
    
    private var currentChar: Character! { return self.cursor<codeLength ? self.code[self.cursor] : nil }

    
    private func readVocabulary() { // scan a sequence of words until the first full [and longest] operator match is made (caveat: if it's not left and right auto-delimiting, need to discard if there are adjoining words) -- actually not quite correct, since numeric words are also significant
        if DEBUG {print("******* START VOCAB LOOP \(self.cursor)")}
        var words = [UnquotedWord]() // used to construct command name, if first word[s] are not numeric word or operator name
        var isFirstWord = true
        
        var currentLongestOperator: [Token]? = nil // the longest operator found so far; TO DO: this is unnecessary complexity (and subtly incorrect) as fullSymbolOperatorMatch!=nil is sufficient to signal longest symbol op has been found; for now though, just evolving towards the API we do want
        
        // TO DO: add `fullSymbolOperatorMatch: FullSymbolOperatorMatch`
        
        var partialPhraseOperatorMatches = [PartialPhraseOperatorMatch]()
        var fullPhraseOperatorMatch: FullPhraseOperatorMatch? = nil
        
        // loop over each word in sequence (sub-loops iterate chars and spaces) until a numeric or longest operator match is made, or all words are consumed
        while self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor]) {
            let wordStartIndex = self.cursor
            
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // pattern match current word for numbers, units, currency, etc.
            // (note: this is done before operator matching to ensure operators cannot redefine numeric words)
            // check for numeric word
            if let (numericValue, endIndex) = readNumericWord(self.code, startIndex: wordStartIndex, numericUnits: self.numericUnits) {
                
                self.cursor = endIndex // TO DO: confirm this positions correctly
                if self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor]) {
                    // TO DO: FIX!!! need to check for any remaining chars in word after numeric including any recognized unit suffix (e.g. `g`) has been consumed. If found, the first char MUST be [the start of] a symbol operator (e.g. `2.5g*count`), otherwise the whole word must be treated as a malformed 'unknown unit suffix' numeric (e.g. `5gremlin`), as only symbol operators have the right to left-auto-delimit. see also TODOs for `isFollowedByAutoLeftDelimitedOperator`
                    print("CAUTION: found additional chars after `\(numericValue)` in word; these are not yet processed correctly")
                }
                // TO DO: it'd be better to copy the way ops are handled and store the matched numeric and its preceding words in top-level var which is processed once we break out of loop; that will avoid this duplication of following logic at cost of one more nil check (which is nothing)
                // we've found a numeric word, but there's already an operator waiting to be added to cache so we have to add that first
                if let operatorTokens = currentLongestOperator {
                    //print("found number, so processing previous operator")
                    self.currentTokensCache.append(contentsOf: operatorTokens)
                    
                    self.cursor = operatorTokens.last!.range.upperBound // TO DO: don't do this as it's wasteful re-reading already processed words; instead, get rid of else clause and remove consumed words from `words` array (a simple `map` should do) before proceeding to add UnquotedName
                } else {
                    if words.count > 0 { // complete and cache tokens for all preceding chars
                        //print("found number, so processing previous words")
                        self.currentTokensCache.append(Token(type: .unquotedName, value: self.joinWords(words), range: self.wordsRange(words)))
                    }
                    let range = wordStartIndex..<endIndex
                    let token = Token(type: .numericWord, value: String(self.code[range]), range: range, numericInfo: numericValue)
                    //if DEBUG {print("READ NUMERIC: \(token)")}
                    self.currentTokensCache.append(token)
                }
                return
            }
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // pattern match for symbol and phrase operators
            // match in-word symbol operators as the word is being read, then match phrase operators once the word is complete
            // TO DO: since symbol ops are read first, there's no point adding them to phrase ops table as well, so can remove that duplication and related logic in ops-table and isValidOp()
            var partialSymbolOperatorMatches = [PartialSymbolOperatorMatch]()
            var fullSymbolOperatorMatch: FullSymbolOperatorMatch? = nil
            var wordChars = "" // the following loop appends Characters to this until it has the entire word
            var isFirstChar = true // used to determine if an in-word symbol op is explicitly left-delimited (i.e. the first char in the word)
            var isDone = false // becomes true once longest match is made, allowing the loop to break early (any remaining words in sequence will be processed as new token[s] on next pass)
            repeat {
                let charIndex = self.cursor
                let char = self.code[charIndex]
                //print("READ VOCAB CHAR `\(char)`")
                self.cursor = self.code.index(after: self.cursor)
                if fullPhraseOperatorMatch == nil { // if there is not already a full phrase operator match, then give symbol operator a chance
                    if updateOperatorMatches(self.operatorsTable.symbols,
                                             partialMatches: &partialSymbolOperatorMatches, fullMatch: &fullSymbolOperatorMatch,
                                             value: char, startIndex: charIndex, endIndex: self.cursor, // TO DO: group as `thisMatch` tuple for clarity?
                                             precedingWords: words, currentPartialWord: (wordChars, wordStartIndex, self.cursor),
                                             isLeftDelimited: isFirstChar, isRightDelimited: self.isEndOfWord) {
                            isDone = true
                            // TO DO: break?
                    }
                    if DEBUG {if (fullSymbolOperatorMatch != nil) {print("MATCHED SYMBOL OPERATOR!!!!", currentLongestOperator as Any)}}
                }
                isFirstChar = false
                wordChars.append(char)
            } while self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor])
            if DEBUG {print("ended char scan loop on \(self.cursor): `\(self.currentChar)`")}
            // note: while in-word symbols are matched first (as chars are being read), if the entire word makes a full phrase match then that takes precedence
            let word = wordChars
            let wordEndIndex = self.cursor // caution: non-inclusive; use `..<` (not `...`) to construct ScriptRange
            if updateOperatorMatches(self.operatorsTable.phrases,
                                     partialMatches: &partialPhraseOperatorMatches, fullMatch: &fullPhraseOperatorMatch,
                                     value: word.lowercased(), startIndex: wordStartIndex, endIndex: wordEndIndex,
                                     precedingWords: words, currentPartialWord: nil,
                                     isLeftDelimited: isFirstWord, isRightDelimited: self.isEndOfPhrase) {
                    isDone = true
            }
            // TO DO: this logic is still subtly wrong: the longest symbol should only become final match if the phrase op doesn't make it; the symbol op still needs to wait until the current partial phrase op either fully matches or fails before it can step in; fix is to get rid of `currentLongestOperator` and put a `longestFullSymbolOperatorMatch` var outside main loop; if there's a partial phrase match underway then the successfully matched symbol op gets stored there, and it's only when the full phrase match succeeds or fails that the decision is made whether or not to use it
            if let match = fullPhraseOperatorMatch {
                currentLongestOperator = self.tokensForMatchedOperator(match.match, fixity: match.fixity)
                words.removeAll()
            } else if let match = fullSymbolOperatorMatch {
                currentLongestOperator = self.tokensForMatchedOperator(match.match, fixity: match.fixity)
                words.removeAll()
            }
            if isDone { break } // no longer matches remain
            isFirstWord = false
            words.append((word, wordStartIndex, wordEndIndex))
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // skip over whitespace
            //if DEBUG {print("!!!Going to skip whitespace now `\(self.code[self.cursor])`")}
            while self.cursor < self.codeLength && Lexer.nonBreakingWhiteSpace.contains(self.code[self.cursor]) {
                //print("SKIP c\(self.cursor)")
                self.cursor = self.code.index(after: self.cursor)
            }
            //if DEBUG {print("ended whitespace skip loop on \(self.cursor): `\(self.code[self.cursor])`")}
            
        }
        
        if DEBUG {print("******* ENDED VOCAB LOOP \(self.cursor)\n\n\t\(words)")}
        
        // TO DO: rather than adding directly to cache, return Array<Token>, allowing this method to be called recursively; that should allow isFollowedByAutoLeftDelimitedOperator to be implemented as recursive call that returns either the required token (plus any others it's had to read ahead to determine the result) or nil
        
        
        // if a full operator match was found, add it
        if let operatorTokens = currentLongestOperator {
            if DEBUG {print("processing remaining operator")}
            self.currentTokensCache.append(contentsOf: operatorTokens)
            // move cursor back to end of operator
            let n = self.cursor
            self.cursor = operatorTokens.last!.range.upperBound
            if DEBUG {print("...MOVED CURSOR FROM \(n) BACK TO: \(self.cursor), `\(self.currentChar)`")}
            return
        }
        
        //
        if words.count > 0 {
            if DEBUG {print("so processing remaining words")}
            self.currentTokensCache.append(Token(type: .unquotedName, value: self.joinWords(words), range: self.wordsRange(words)))
            self.cursor = words.last!.endIndex
        }
    }
    
    
    
    private func readUnquotedName() {
        // TO DO: numeric words should still be treated as self-delimited tokens
        // TO DO: implement; read all whitespace-separated words and append .UnquotedName token
        var words = [String]()
        let startIndex = self.cursor
        var endWordIndex: ScriptIndex = startIndex
        while self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor]) {
            let startWordIndex = self.cursor
            // read to end of word
            while self.cursor < self.codeLength && !Lexer.reservedCharacters.contains(self.code[self.cursor]) {
                self.cursor = self.code.index(after: self.cursor)
            }
            words.append(String(self.code[startWordIndex..<self.cursor]))
            endWordIndex = self.cursor
            while self.cursor < self.codeLength && Lexer.nonBreakingWhiteSpace.contains(self.code[self.cursor]) {
                self.cursor = self.code.index(after: self.cursor)
            }
        }
        self.cursor = endWordIndex // TO DO: confirm this positions correctly
        //print("READ UNQUOTED NAME \"\(String(self.code[startIndex..<endWordIndex]))\" \(startIndex..<endWordIndex)")
        return self.currentTokensCache.append(Token(type: .unquotedName, value: words.joined(separator: " "), range: startIndex..<endWordIndex))
    }
    
    
    //**********************************************************************
     // Tokenizer
    
    
    private(set) var currentTokensCache: [Token] = [gStartOfCodeToken] // DEBUG: parser's debug logging currently sneaks a peek // TO DO: make fully private
     
     
    // read next token[s] into currentTokensCache; inserts gEndOfCodeToken once all tokens have been read
    private func readNextToken(_ ignoreVocabulary: Bool) {
        if DEBUG {print("READ_NEXT_TOKEN \(self.cursor) ==================================================")}
        // skip any leading 'non-breaking' white space (note: unlike LF, space and TAB do not automatically delimit unquoted text but instead can be treated as part of it, allowing the lexer to combine white space-separated words as a single .Operator/.UnquotedName token if/where/when it wishes. Inter-word tabs/spaces will be read and normalized by the relevant `Lexer.read...` method; tab/space runs appearing elsewhere in the code delimit tokens normally and are otherwise ignored.)
        while self.cursor < self.codeLength && Lexer.nonBreakingWhiteSpace.contains(self.code[self.cursor]) {
            self.cursor = self.code.index(after: self.cursor)
        }
        if self.cursor < self.codeLength { // TO DO: what to do if overrun? (e.g. endlessly adding null tokens prob. isn't good idea if something gets stuck in infinite loop)
            let start = self.cursor
            let firstChar = self.code[self.cursor] // TO DO: prob make this `char` var and rejig stages into a single switch
            // 1. quoted text/name
            if let token = Lexer.quoteDelimiters[firstChar] { // found open quote
                var chars = ""
                var isquote = true
                while isquote {
                    self.cursor = self.code.index(after: self.cursor) // eat open quote
                    let substart = self.cursor // [1] if already starting inside quote, need to initialize token, text, isquote, and start vars, skip eating open quote (since there isn't one), then continue from this line
                    while self.cursor < self.codeLength && Lexer.quoteDelimiters[self.code[self.cursor]] != token {
                        self.cursor = self.code.index(after: self.cursor)
                    }
                    if self.cursor == self.codeLength {
                        chars.append(contentsOf: self.code[substart..<self.cursor])
                        return self.currentTokensCache.append(Token(type: token, value: chars, range: start..<self.cursor, partial: +1))
                    }
                    chars.append(contentsOf: self.code[substart..<self.cursor])
                    self.cursor = self.code.index(after: self.cursor) // eat close quote
                    isquote = self.cursor < self.codeLength && Lexer.punctuation[self.code[self.cursor]] == token
                    if isquote { // note: quote chars are escaped by typing twice (the first is ignored, the second used)
                        chars.append(self.code[self.cursor])
                    }
                }
                return self.currentTokensCache.append(Token(type: token, value: chars, range: start..<self.cursor))
            }
            // 2. unquoted word (i.e. anything that isn't quoted, punctuation, or white space; though once called the read function may scan/consume those too where appropriate)
            if !Lexer.reservedCharacters.contains(firstChar) {
                if ignoreVocabulary {
                    self.readUnquotedName()
                } else {
                    self.readVocabulary()
                }
                return
            }
            // 4. «...» annotations are atomic and nestable and read as-is (understanding their contents is left to the caller)
            if let t = Lexer.annotationDelimiters[firstChar] {
                if t < 0 { // TO DO: how to handle case where `»` is encountered first?
                    return self.currentTokensCache.append(Token(type: .annotationLiteralEnd, value: String(firstChar), range: start..<self.cursor))
                }
                let start = self.cursor
                var level = 1
                while level > 0 {
                    self.cursor = self.code.index(after: self.cursor)
                    if self.cursor == self.codeLength {
                        let range = start..<self.cursor
                        return self.currentTokensCache.append(Token(type: .annotationLiteral, value: String(self.code[range]), range: range, partial: level))
                    }
                    level += Lexer.annotationDelimiters[self.code[self.cursor]] ?? 0
                }
                self.cursor = self.code.index(after: self.cursor)
                let range = start..<self.cursor
                return self.currentTokensCache.append(Token(type: .annotationLiteral, value: String(self.code[range]), range: range))
            }
            // 5. all other tokens (single-char)
            if let token = Lexer.punctuation[firstChar] {
                self.cursor = self.code.index(after: self.cursor)
                return self.currentTokensCache.append(Token(type: token, value: String(firstChar), range: start..<self.cursor))
            }
        }
        return self.currentTokensCache.append(gEndOfCodeToken)
    }
    
    
    //**********************************************************************
     // Public interface
    
    
    private(set) var currentTokenIndex = 0 // parser can get this value and use it to backtrack to that token later on if required; caution: only valid until the next flush() call
    
    
    var currentToken: Token { return self.currentTokensCache[self.currentTokenIndex] } // current token
    
    // TO DO: FIX!!!!! ignoreVocabulary will screw up cache if it changes; safe solution is to write all tokens to cache with a flag that indicates the ignore settings with which they were originally read; advance, etc. can then check the token has the same ignore settings as it has, and flush it and the remaining tokens if not
    
    func advance(ignoreVocabulary: Bool = false) {
        self.currentTokenIndex += 1
        if self.currentTokenIndex == self.currentTokensCache.count { self.readNextToken(ignoreVocabulary) }
        //            if self.currentTokenIndex > 100 {print("BUG: punc.advance() is stuck: "+(self.currentTokensCache.map{String($0)}.joinWithSeparator("\n\t"))); break} // TO DO: DEBUG; DELETE
    }
    
    // TO DO: option to skip next N tokens?
    func skip(_ tokenType: TokenType) throws { // advance to next token, throwing SyntaxError if it's not the specified type
        self.advance()
        if self.currentToken.type != tokenType {
            throw SyntaxError(description: "[0] Expected \(tokenType) but found \(self.currentToken.type): `\(self.currentToken.value)`")
        }
    }
    
    func backtrackTo(_ tokenIndex: Int, flush: Bool = false) { // backtrack to a previous token, optionally flushing subsequent tokens if they need to be regenerated (e.g. with different ignoreVocabulary option)
        self.currentTokenIndex = tokenIndex
        if flush {
            self.currentTokensCache.removeSubrange(tokenIndex+1..<self.currentTokensCache.count)
            self.cursor = self.currentToken.range.upperBound
        }
    }
    
    
    // TO DO: CAUTION: lookahead will screw up cache if ignoreVocabulary changes, as tokens appended to cache read with one ignore setup need to be thrown out if read with another; currently this shouldn't be a problem since only parseRecord does this, and it backtracks and flushes before re-reading with different ignoreVocabulary setting
    
    func lookahead(by offset: UInt, ignoreVocabulary: Bool = false) -> Token { // TO DO: what about annotations? (should prob. also ignore those by default, but need to confirm)
        if offset == 0 { return self.currentToken }
        var count: UInt = 0
        var lookaheadTokenIndex: Int = self.currentTokenIndex
        //if DEBUG {print("LOOKING AHEAD from \(self.currentToken) by \(offset)")}
        while count < offset { // TO DO: .WhiteSpace tokens are no longer emitted, so this loop can be simplified
            lookaheadTokenIndex += 1
            while lookaheadTokenIndex >= self.currentTokensCache.count { self.readNextToken(ignoreVocabulary) }
            let lookaheadToken = self.currentTokensCache[lookaheadTokenIndex]
            if lookaheadToken.type == .endOfCode {
                //if DEBUG {print("LOOKAHEAD REACHED END: \(self.currentTokensCache.map{"\($0.value)"})")}
                return lookaheadToken
            }
            count += 1
        }
        //print("LOOKED AHEAD to    \(self.currentTokensCache[lookaheadTokenIndex])")
        return self.currentTokensCache[lookaheadTokenIndex]
    }
    
    func flush() { // clear cache of fully-parsed tokens (at minimum, this leaves the current token at index 0)
        self.currentTokensCache.removeSubrange(0..<self.currentTokenIndex)
        self.currentTokenIndex = 0
    }
}








