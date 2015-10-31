//
//  operator-lookup-table.swift
//  entoli
//
//

// note: icon's success/failure result allows chaining of ops, e.g. `3 < 5 < 8`; the first op returns RH operand or failure, which second op consumes

// TO DO: symbol ops should really be in another table, as they need to be recognized when they appear _within_ words too (it's still worth including them here, as pretty printer will tend to insert whitespace around them, making them much easier to parse subsequently); furthermore, it would probably make sense to group related ops into separate sets (perhaps stored in a dict that identifies each set by unique name), allowing clients to selectively load only those ops they actually want/need, e.g. `without using {"org.entoli.math-operators"}` at top of a script would unload the standard math ops (which are normally loaded by default for convenience) // TO DO: actually, it might be better to have a single table and determine whether op is symbol or name according to char(s) it contains



// TO DO: split into 2 [generic] classes for Keyword- and Symbol-based ops?

class OperatorDefinitions {
    // given a multi-word operator name, split it into words, then store as a series of nested dicts, ending in the operator definition itself

    enum OperatorForm {
        case Atom
        case Prefix
        case Infix
        case Postfix
        
        var hasLeftOperand: Bool { return (self == .Infix || self == .Postfix) }
    }
    
    typealias OperatorDefinition = (name: String, aliases: [String], precedence: Int, form: OperatorForm)
    
    static let SymbolOperators: [OperatorDefinition] = [ // note: ops also need to indicate if explicit separators are needed on their prefix and/or postfix sides for them to be parsed as ops rather than part of larger phrase, or if they should always be parsed as ops regardless of what surrounds them (note that quoting their names will cause them to be read as ordinary command names; the op tables simply promote them in parsing)
        
        
        // these also need split into chars for in-word detection
        
        // TO DO: how to disambiguate overloaded op/command names? e.g. `'-'{1}` (neg) might be distinguished from `'-'{2,1}` (sub) by counting args, but that won't work where a prefix op overloads a postfix op; also, in the `-` case, `'-'{1,2}` would be a more correct representation (`-1` and `2-1` both negate the RH operand, not the left, but record items count left to right so the required value wants to be first, followed by the optional one), but then that causes more confusion (plus, of course, there's also `2;'-'1` to take into account, and even `1;'-'`)
        
        // arithmetic
        ("^", [], 500, .Infix),     //parserightinfixop // TO DO: what to use as exponent operator? (`^`, `exp`?)
        ("+", [], 490, .Prefix),    //parseprefixop
        ("-", ["–"], 490, .Prefix), //parseprefixop  // note: also accepts n-dash as synonym
        ("*", [], 480, .Infix),     //parseinfixop
        ("/", [], 480, .Infix),     //parseinfixop
        ("+", [], 470, .Infix),     //parseinfixop
        ("-", ["–"], 470, .Infix),  //parseinfixop  // note: also accepts n-dash as synonym
        
        
        // numeric comparisons (non-ASCII symbols have ASCII aliases for alternative input)
        (">", [], 400, .Infix),
        ("<", [], 400, .Infix),
        ("=", ["=="], 400, .Infix),
        ("≠", ["!="], 400, .Infix),
        ("≤", ["<="], 400, .Infix),
        ("≥", [">="], 400, .Infix),
    
        // concatenation
        ("&", [], 450, .Infix),
    ] // note: symbol ops should be added to both keyword (for speedy whole-word lookup) and symbol op (for in-word lookup) tables
    
    
    static let KeywordOperators: [OperatorDefinition] = [ // note: ops also need to indicate if explicit separators are needed on their prefix and/or postfix sides for them to be parsed as ops rather than part of larger phrase, or if they should always be parsed as ops regardless of what surrounds them (note that quoting their names will cause them to be read as ordinary command names; the op tables simply promote them in parsing)

        //  TO DO: how best to indicate preferred form to be used by pretty-printer? (inclined to have PP always use correct form; ASCII forms are really only provided as keyboard shortcuts, so it's users' problem if their crappy computer can't do UTF8 by now)
        
        // caution: following patts good example of why correct pattern order is critical: several tokens starts with `is` but longest match must *always* be looked for first
        
        // arithmetic
        ("div", [], 480, .Infix),
        ("mod", [], 480, .Infix),

        
        // non-numeric comparisons (e.g. text); note that parsefunc overrides standard precedence to allow `COMP as TYPE` to specify comparison type, e.g. `A is B as C` is shorthand for `(A as C) is (B as C)` (currently, `as` operator binds lowest, so would apply to comparison's result, but since these ops always return boolean that isn't really useful, whereas applying cast to both operands prior to comparison is, and allows things like case-insensitive text comparisons and list of X comparisons to be done as well) -- note that a failed cast will throw error (not sure if catching this should be done by typespec, and if it is then what's to prevent `A is not B as C` returning true when both A and B fail to cast causing typespec to supply identical default value for each)
        // TO DO: what about `eq`, `lt`, etc. aliases? should `is [same [as]]` be accepted? (toss-up between keeping it forgiving and learnable, though bear in mind that all aliases should automatically convert to canonical form when pretty-printed - they are purely for input convenience, not to riddle users' scripts with synonyms)
        ("is before", [], 400, .Infix),
        ("is equal or before", ["is equal to or before", "is before or equal to", "is not after"], 400, .Infix),
        ("is after", [], 400, .Infix),
        ("is equal or after", ["is equal to or after", "is after or equal to", "is not before"], 400, .Infix),
        ("is not", ["is not equal to"], 400, .Infix),
        ("is", ["is equal to"], 400, .Infix),
        
        // Boolean
        ("not", [], 100, .Prefix), // TO DO: should AND, OR, and NOT be pretty-printed in [small]caps? (If so, how/where best to define custom formatting hints? [note: while the quick answer would be here in ops table, this is really part of a much larger, more general question as to how formatting should be applied to ALL code; e.g. according to high-level semantics such as categories of procedures and data relationships rather than superficial syntactic structure; e.g. given an obvious list literal, is it not more useful for highlighting to indicate where it came from or what it is used for, rather than just reiterate 'bracket,string,comma,string,comma,...,bracket'?])
        ("and", [], 99, .Infix),
        ("or", [], 98, .Infix),
        
        // cast
        ("as", [], 80, .Infix),
        
        // eval clauses
        ("thru", ["through"], 50, .Infix),
        ("catching", [], 50, .Infix),
        ("else", [], 50, .Infix),
    ]
    
    typealias KeywordOperatorWords = [String:KeywordOperatorWord]
    typealias SymbolOperatorWords = [Character:SymbolOperatorWord]

    struct KeywordOperatorWord {
        var definition: OperatorDefinition? = nil // TO DO: need separate slots for prefix/atom (i.e. no LH operand) and infix/postfix (i.e. has LH operand) (note: multifix is variant on either); alternatively, use separate tables (which would make more sense, since parser looks up each separately)
        var nextWords: KeywordOperatorWords = [:]
    }

    struct SymbolOperatorWord {
        var definition: OperatorDefinition? = nil // TO DO: need separate slots for prefix/atom (i.e. no LH operand) and infix/postfix (i.e. has LH operand) (note: multifix is variant on either); alternatively, use separate tables (which would make more sense, since parser looks up each separately)
        var nextWords: SymbolOperatorWords = [:]
    }
    
    private(set) var prefixKeywordDefinitions: KeywordOperatorWords = [:]
    private(set) var infixKeywordDefinitions:  KeywordOperatorWords = [:]
    
    private(set) var prefixSymbolDefinitions: SymbolOperatorWords = [:]
    private(set) var infixSymbolDefinitions:  SymbolOperatorWords = [:]
    
    private func addOperator(var words: [String], inout table: KeywordOperatorWords, definition: OperatorDefinition) {
        let word = words.removeFirst()
        if table[word] == nil {
            table[word] = KeywordOperatorWord()
        }
        if words.count > 0 {
            self.addOperator(words, table: &(table[word]!.nextWords), definition: definition)
        } else {
            if table[word]!.definition != nil {
                print("Duplicate operator entry \(word)") // looks like an operator with the same name and *fix has already been added // TO DO: how best to deal with this?
            }
            table[word]!.definition = definition
        }
    }
    
    func addOperator(definition: OperatorDefinition) {
        for name in [definition.name] + definition.aliases {
            // TO DO: what about normalizing name? (trim, lowercase, etc)
            // Swift's dogawful 'copy by value' plus inability to use `&` on vars is a huge PITA
            let chars = name.characters.split{$0 == " "}.map(String.init)
            if definition.form.hasLeftOperand {
                self.addOperator(chars, table: &self.infixKeywordDefinitions, definition: definition)
            } else {
                self.addOperator(chars, table: &self.prefixKeywordDefinitions, definition: definition)
            }
        }
    }
    
    private func addOperator2(var chars: [Character], inout table: SymbolOperatorWords, definition: OperatorDefinition) {
        let word = chars.removeFirst()
        if table[word] == nil {
            table[word] = SymbolOperatorWord()
        }
        if chars.count > 0 {
            self.addOperator2(chars, table: &(table[word]!.nextWords), definition: definition)
        } else {
            if table[word]!.definition != nil {
                print("Duplicate operator entry \(word)") // looks like an operator with the same name and *fix has already been added // TO DO: how best to deal with this?
            }
            table[word]!.definition = definition
        }
    }
    

    func addOperator2(definition: OperatorDefinition) {
        for name in [definition.name] + definition.aliases {
            // TO DO: what about normalizing name? (trim, lowercase, etc)
            // Swift's dogawful 'copy by value' plus inability to use `&` on vars is a huge PITA
            let chars = Array(name.characters)
            if definition.form.hasLeftOperand {
                self.addOperator2(chars, table: &self.infixSymbolDefinitions, definition: definition)
            } else {
                self.addOperator2(chars, table: &self.prefixSymbolDefinitions, definition: definition)
            }
        }
    }
    
    init() {
        for operatorDefinition in self.dynamicType.SymbolOperators + self.dynamicType.KeywordOperators {
            self.addOperator(operatorDefinition)
        }
    }

}