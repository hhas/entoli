//
//  datatypes.swift
//  entoli
//
//
//

import Foundation

// TO DO: what about implementing `evaluate()->Value` funcs as dynamic alternative to the static `evaluate<ReturnType>()->ReturnType.SwiftType`?

// TO DO: what about implementing NativeCoercion protocol that can be used as parameter type instead of `Coercion` (which can't coerce) and `SwiftCast` (which requires generics)


// TO DO: if/when Swift supports `override` in extensions, move cast and evaluation methods to extensions

// TO DO: should `description` return entoli literal representation and `debugDescription` return Swift literal representation? (e.g. would make error messages easier to build)

// note: KISS; standard AST-walking eval doesn't need to be (and shouldn't be) optimized, and consistency+absence of 'shortcuts' will avoid creating hidden behavioral 'holes' that tools such as interactive viewers and code generators can't analyse (e.g. expression group should always coerce items to commands, even though it's not very efficient, as that method call can be sniffed by value-like objects as well as values)

// `foo as anything` calls `foo` proc and returns result; `foo as expression` returns `foo` command; `foo as procedure` looks up `foo` name and returns closure

// TO DO: pretty sure we need `NAME as text` and `TEXT as name` casts (though need to be careful about not applying them as coercions anywhere the distinction matters, e.g. "foo" in a command context should eval as itself, not call `foo` proc)

// also need to think about how AOT compilation might change code behavior (since all movements from entoli to Swift datatypes will require primitive coercions to specific types, where previously entoli code might accept any type)

// TO DO: how best to support selective stepped eval (e.g. step by command/block); simplest would be to transform code before execution (e.g. using SteppedExpressionSequence in place of ExpressionBlock), rather than passing flags which ExpressionBlock must always check


// note: value-to-value conversion methods have `as` prefix and should be supported on all objects (throwing a CoercionError if not appropriate)
// methods to convert standard types to special types (including non-values) have `to` prefix, e.g. `toScalar`; these appear only on objects to which they apply

// TO DO: all values should have a slot containing Coercion object describing their canonical type (e.g. Text contains TextCoercion, List contains ListCoercion(gAnyValueCoercion)); question is what to do when more than one is acquired, e.g. as value passes thru additional coercions - might want to keep all such coercions in an internal Set so that subsequent passes of that value require only a `contains` test, while allowing the best type to be calculated lazily (e.g. whole number) if/when that first check doesn't give a match

    // TO DO: a named pair _must_ evaluate as `store`, except in list (where both operands are evaled as exprs and a key-value pair returned), record (remains as a name-value pair, with eval of right operand determined by arg/expr context), or [parensed?] pair value (remains as pair, with coercion context determining eval)

// TO DO: implement ExpressibleByStringLiteral, etc.



class Value: CustomStringConvertible, CustomDebugStringConvertible { // TO DO: should this be named Expression, or have Expression protocol? (i.e. are there any Value classes that *can't* be evaluated as atomic/group expressions?)
    
    var annotations = [Any]() // TO DO: structure, API, literalRepresentation
    
    // literal representations // TO DO: implement `literalRepresentation` methods that take additional options (e.g. always/never quote names) for formatting values as entoli literals
    
    var description: String {return "\(type(of: self))(...)"} // subclasses should override to return short developer-readable description (eventually this might just call renderer)
    
    var debugDescription: String {return "\(type(of: self))(...)"} // subclasses should override to return their Swift literal representation
    
    var typename: String {return String(describing: type(of: self)) } // TO DO: this should return native entoli type name (`text`, `name`, `expression`, etc)
    
    
    // conversion; Value.toTYPE() methods allow parser to perform raw type conversions on Values without evaluation
    
    func toCommand() throws -> Command { throw CastError(value: self, type: Command.self) }
    
    // evaluation
    //
    // note: _expandAs... methods are used by Coercions to convert between standard entoli Values (text,name,command,pair,list,record), resolving any unevaluated expressions as necessary
    // TO DO: rather than throw, should most/all of these call _expandAsType_ catchall, which can then throw CoercionError by default; this would allow e.g. GroupExpression to override _expandAsType_() rather than evaluate(), which might be safer
    
    func _expandAsAny_(_ env: Scope) throws -> Value { // TO DO: note that `returnType: Coercion` may or may not be a NativeCoercion (both native and Swift coercions call _expand_ methods)
        fatalNotYetImplemented(self, #function)
    }
    func _expandAsText_(_ env: Scope) throws -> Text {
        throw ExpansionError.unsupportedType
    }
    func _expandAsName_(_ env: Scope) throws -> Name {
        throw ExpansionError.unsupportedType
    }
    // TO DO: need _expandAsTuplePair_, and implement _expandAsPair_ based on that
    func _expandAsPair_<KeyType, ValueType>(_ env: Scope, keyType: KeyType, valueType: ValueType) throws -> Pair
                                            where KeyType: Coercion, KeyType: SwiftCast, KeyType.SwiftType: Value,
                                                ValueType: Coercion, ValueType: SwiftCast, ValueType.SwiftType: Value {
        throw ExpansionError.unsupportedType
    }
    func _expandAsArray_<ItemType>(_ env: Scope, itemType: ItemType) throws -> [ItemType.SwiftType]
                                    where ItemType: Coercion, ItemType: SwiftCast {
        return [try self.evaluate(env, returnType: itemType)]
    }
    func _expandAsList_(_ env: Scope, itemType: NativeCoercion) throws -> List {
        return List(items:  [try self.evaluate(env, returnType: itemType)], itemType: itemType)
    }
    
    
    func _expandAsRecord_(_ env: Scope) throws -> Record { // need to pass fieldTypes or similar if expanding here
        // TO DO: what about passing `type:RecordSignature?` or is it simpler to convert to record first, then call Record.toRecordSignature to map fields to template? (since no. of fields are small, it probably doesn't make all that much difference); note: main problem with passing signature is that all fields need to be native types
        return Record(self)
    }
    func _expandAsCommand_(_ env: Scope) throws -> Command { // names and commands can coerce to commands (note: while named pairs are cast to 'store' commands in certain evaluation scopes, this is parser's job, not runtime's, so Pair does not implement _expandAsCommand_)
        throw ExpansionError.unsupportedType
    }
    
    func _expandAsCoercion_(_ env: Scope) throws -> Coercion {
        throw ExpansionError.unsupportedType
    }
    
    // Value.evaluate() is the standard entry point for evaluating any given value // TO DO: rename `evaluate(as returnType: ReturnType, in env: Scope)`?
    // TO DO: evaluate methods should check if value already contains cached representation for the given returnType
    // TO DO: evaluate methods should trap coercion sub-errors and rethrow as permanent CoercionError (Q. should they also rethrow chained CoercionErrors to provide full trace to failed item, e.g. in lists and records?)
    func evaluate<ReturnType>(_ env: Scope, returnType: ReturnType) throws -> ReturnType.SwiftType where ReturnType: SwiftCast {
        return try returnType._coerce_(self, env: env)
    }
    func evaluate(_ env: Scope, returnType: NativeCoercion) throws -> Value {
        return try returnType._coerce_(self, env: env)
    }
}


//**********************************************************************
//

// note: suspect special names will only work as Name subclasses (or as dedicated subclass[es] of Value) if they're also defined as operators

class NullValue: Value { // TO DO: need to think about how 'constant' names work; need to be defined as [.StoredValue] command in appropriate scope that returns Name; also need to decide if any of these 'constants' are important enough to define as atoms (probably not, as that'd create inconsistency between constant names that are defined as built-ins vs those defined by libraries, e.g. scriptable apps)
    
    // literal representations
    
    let keyString = "nothing" // TO DO: what should native name be? `nothing`? `missing value`? what else...`did nothing`? `test failed`, `evaluation error` [`command failed`?]; note that these 'special' built-in names *must* be reserved in Scope so that users can't accidentally override them (though be aware that named pairs in records still allow the same names to be used as [literal] names on LHS, so will need to give some thought to that [probably correct behavior, since parameter records should permit any keys, even when the same words would have special meaning elsewhere]; though given Swift's stubborn refusal to allow initializers to return anything except specified type there could still be holes where a 'special' name ends up as an ordinary Name instance, so we're going to have to think about suitability/safety of using anything except Name type to represent them, otherwise risk is that primitive code tests for `...is NullValue` rather than `keystring ==...`, which it will want to do for efficiency; suppose if 'special' names are fixed then an enum could be assigned internally within Name class to indicate if name is special or non-special, in which case equality tests should rely on that; this should also not affect record keys; expand methods below would need to check enum to decide what, if any, coercions are allowed, which again is less convenient; the alternative would be for most Name constructors to respect `reservedNames` set and refuse to construct 'special' names by default); fwiw, other possibility might be to make all Name constructors private, forcing client code to use classfuncs that are guaranteed to return the correct [sub]class for any given name string
    
    override var description: String { return self.keyString }
    override var debugDescription: String { return "gNullValue" }
    
    // ugh; given that `nothing` serves a special role (default argument handling), should it be a unique class and defined as operator? (main concern is that it must never be masked by a command; probably need a set of "forbidden names" in Scope.store())
    
    override func _expandAsAny_(_ env: Scope) throws -> Value { throw ExpansionError.nullValue }
    override func _expandAsText_(_ env: Scope) throws -> Text { throw ExpansionError.nullValue }
    override func _expandAsName_(_ env: Scope) throws -> Name { throw ExpansionError.nullValue }
    
    // TO DO: simplify sig by grouping key and value types into tuple
    override func _expandAsPair_<KeyType, ValueType>(_ env: Scope, keyType: KeyType, valueType: ValueType) throws -> Pair
                                                    where KeyType: Coercion, KeyType: SwiftCast, KeyType.SwiftType: Value,
                                                        ValueType: Coercion, ValueType: SwiftCast, ValueType.SwiftType: Value {
        throw ExpansionError.nullValue
    }
    override func _expandAsArray_<ItemType>(_ env: Scope, itemType: ItemType) throws -> [ItemType.SwiftType]
                                            where ItemType: Coercion, ItemType: SwiftCast {
        throw ExpansionError.nullValue
    }
    override func _expandAsRecord_(_ env: Scope) throws -> Record { throw ExpansionError.nullValue }
    override func _expandAsCommand_(_ env: Scope) throws -> Command { throw ExpansionError.nullValue }
}

let gNullValue = NullValue() // note: the main reason for this being a 'special' value rather than just, say, an empty record is so that it will throw NullValueCoercionErrors within Coercions which CoercionModifiers, e.g. `DefaultValue` can easily intercept and replace with default value (c.f. kiwi); however, see notes about about potential problems with reliably distinguishing special from non-special names


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
        let code = self.string.characters
        let startIndex = skipWhiteSpace(code, startIndex: code.startIndex)
        guard let (numeric, idx) = readNumericWord(code, startIndex: startIndex, numericUnits: numericUnits) else {
            throw CoercionError(value: self, description: "Not a numeric value.")
        }
        if skipWhiteSpace(code, startIndex: idx) != code.endIndex { // found a numeric, but it was followed by more characters
            throw CoercionError(value: self, description: "Not a numeric value (unknown characters at end).")
        }
        return numeric
    }
    
    func toScalar() throws -> Scalar {
        let numeric = try self.toNumeric()
        switch numeric {
        case .number(let scalar):
            return scalar
        default:
            throw CoercionError(value: self, description: "Not a number: \(numeric)")
        }
    }
    
    // expansion
    
    override func _expandAsAny_(_ env: Scope) throws -> Value { return self }
    override func _expandAsText_(_ env: Scope) throws -> Text { return self }
    
    
    static func ==(lhs: Text, rhs: Text) -> Bool { // TO DO: all Values should be Equatable
        return lhs.string.lowercased() == rhs.string.lowercased() // TO DO: instead of normalizing each time, call Text.normalizedString, and let Text cache lowercase string for reuse
    }
}


class Name: Value {
    
    let string: String
    let keyString: String // TO DO: ideally this would be calculated on first use then stored, but lazy modifier doesn't work with `self`, so for now just store it on init as well // TO DO: rename `normalizedString`
    
    init(_ string: String) {
        self.string = string
        self.keyString = string.lowercased()
    }
    
    // literal representations
    
    // TO DO: for entoli literal representation, this needs to escape any `'` chars within self.string; in addition, quotes should be omitted if unnecessary (impractical here, since `description` var knows nothing about where it's being used, but full renderer will be able to track details such as nesting depth and lexical context)
    
    override var description: String { return "'\(self.string)'" }
    
    override var debugDescription: String { return "\(type(of: self))(\(self.string.debugDescription))" }
    
    // conversion
    
    override func toCommand() throws -> Command { return Command(name: self) }
    
    // expansion
    
    override func _expandAsAny_(_ env: Scope) throws -> Value { return self }
    override func _expandAsName_(_ env: Scope) throws -> Name { return self }
    override func _expandAsCommand_(_ env: Scope) throws -> Command { return try self.toCommand() }
    override func _expandAsCoercion_(_ env: Scope) throws -> Coercion { // note: returnType is passed here purely for error reporting use, and doesn't support SwiftCast API which evaluate requires,
        return try self.toCommand().evaluate(env, returnType: gTypeCoercion) // ...so for now we just kludge it
    }
}


//**********************************************************************
// composites/collections


class Pair: Value { // TO DO: use generic Pair<KeyT,ValueT> rather than subclassing? (problem is getting various methods to compile, plus `is Pair` won't work if required; may be better to keep this as-is)

    let key: Value, value: Value
    
    init(_ key: Value, _ value: Value) {
        self.key = key
        self.value = value
    }
    
    // literal representations
    
    // TO DO: `A:B:C` is right-associative; formatter may want to parenthesize RH pair for clarity
        
    override var description: String { return "\(self.key): \(self.value)" }
    override var debugDescription: String { return "Pair(\(self.key.debugDescription), \(self.value.debugDescription))" }
    
    // conversion
    
    func toStoreCommand() throws -> Command { // used in `do` block and script expression sequences only
        guard let key = self.key as? Name else { throw BadArgument(description: "Pair value does not have a valid name: \(self)") }
        let command = Command("store", self.value, key) // TO DO: include field names? Pair(Name("value"), self.value), Pair(Name("named"), name)? also, include read-only flag once that's implemented, as `store` command will probably use read-write as default
        command.annotations.append("Format.StoreOperator")
        return command
    }
    
    func toFieldSignature() throws -> FieldSignature { // TO DO: this is no good, as value is [typically?] a command that must be evaluated to Coercion (and may itself be parenthesized), thus it needs to be evaled to obtain Coercion instance, which requires a fully prepared runtime env, so can only be done at the time the procedure is defined
        guard let key = self.key as? Name else { throw BadArgument(description: "Pair value does not have a valid name: \(self)") }
        
        
        // TO DO: FIX!!! while name is literal, value is an expr that needs evaluated to Coercion (note: need to check expression[group]s aren't going to cause problems, or do anything they shouldn't, e.g. side effects; this is probably job for TypeCoercion); all we can do here is call value.toCommand() and cross fingers that when evaled it returns a Coercion value
        guard let value = self.value as? Coercion else { throw BadArgument(description: "Pair value does not have a valid type: \(self)") }
        return FieldSignature(key, value)
    }

    // expansion
    
    // TO DO: would it be possible/practical to parameterize on returnType? (this'd require all _expand... methods to be generics, but assuming it'd work could likely provide a much tidier API since everything could be done using that)
    override func _expandAsPair_<KeyType, ValueType>(_ env: Scope, keyType: KeyType, valueType: ValueType) throws -> Pair
                                                     where KeyType: Coercion, KeyType: SwiftCast, KeyType.SwiftType: Value,
                                                           ValueType: Coercion, ValueType: SwiftCast, ValueType.SwiftType: Value {
        return try Pair(self.key.evaluate(env, returnType: keyType), self.value.evaluate(env, returnType: valueType))
    }
    
    override func _expandAsAny_(_ env: Scope) throws -> Value {
        return try self._expandAsPair_(env, keyType: gAnyValueCoercion, valueType: gAnyValueCoercion) // TO DO: what if key is Name? pretty sure it needs left as-is, otherwise applying two expands to a pair will change its key; FIX
    }
    
    override func evaluate<ReturnType>(_ env: Scope, returnType: ReturnType) throws -> ReturnType.SwiftType
                                        where ReturnType: Coercion, ReturnType: SwiftCast {
        // TO DO: how should this work? (or is it coercion's job to state whether key should be treated as a literal name, or a command, or whatever?)
        fatalNotYetImplemented(self, #function)
    }
    
}



class List: Value { // TO DO: how best to constrain as array/dictionary/set? (parser hints, first usage); e.g. `[1, nothing, []]` might require an explicit `as list of anything` cast if standard behavior is to infer itemType from first item added (alternatively, it could just replace that first guess and force it to be looser as non-matching types are encountered; the question then being should only literals be able to do this, as opposed to values returned by commands or subsequently added to an editable list); conversely, `[1,2,3]` would require an explicit `as list of whole number` to restrict it beyond its automatically inferred `list of text` type
    
    // Q. to what extent should lists do copy-on-write? (note that Swift stdlib includes funcs for determining unique ownship, avoiding need for unnecessary copying); TBH, this question ties in with larger question of immutable vs mutable; another possibility is for collections to be implicitly mutable within their originating scope, then 'sealed' as immutable once they leave that scope unless explicitly declared e.g. `as mutable list`
    
    let items: [Value] // TO DO: prob needs to be enum of Array|Dictionary|Set|Empty (note: `Empty` just means List's internal representation hasn't been determined yet; that should resolve when first item is added, or when a coercion is applied, or possibly if type can be inferred from static analysis [though the first two options are simpler and likely preferred])
    
    
    // TO DO: should the original ListCoercion(...) type spec also be included? (will depend on how mutability works: if min/max are specified for editable list, these should be enforced when list changes size)
    
    private(set) var itemType: NativeCoercion? // TO DO: need to decide how/when this is set/unset/enforced; also, mutability should be part of type
    
    // TO DO: adding a named Pair to a List is problematic, as normal evaluation rules say it should eval as `store`, but that is undesirable in list; best if first item is checked to see if it's a Pair; if it is, and key is Name, store Name as dictionary key (i.e. parenthesizing name will cause it to eval as command, returning whatever value type it likes [including another Name, which is how AS-style 'constants' can be implemented, caveat the parameter-grabbing problem which isn't ideal but should always be caught at runtime as 'too many args' error, and can be ameliorated at edit-time by auto-correct parenthesizing or otherwise punctuating name based on its 'proc' definition])
    
    init(items: [Value], itemType: NativeCoercion? = nil) { // TO DO: problem here is ordered/key-value/unique aren't fully interchangeable
        self.items = items
        self.itemType = itemType
    }
    
    convenience init(_ items: Value...) {
        self.init(items: items)
    }
    
    // literal representations
    
    override var description: String {return "[" + items.map{$0.description}.joined(separator: ", ") + "]"}
    override var debugDescription: String {return "List(" + items.map{$0.debugDescription}.joined(separator: ", ") + ")"}
    
    // evaluation // TO DO: what about expanding as generator/lazy array, for use by primitive procs?
    
    override func _expandAsAny_(_ env: Scope) throws -> Value {
        return try self._expandAsList_(env, itemType: gAnyValueCoercion)
    }
    
    // TO DO: passing itemType as separate param is kinda smelly; would be better to have ListCoercionProtocol; alternatively, might be best to get rid of returnType: entirely and let Coercion._coerce_ tag the returned Value, and Value.evaluate() rethrow with full error details
    // TO DO: get rid of this and provide toArray() method instead? this'd leave calling Coercion to coerce each item, avoiding need to pass itemType (or returnType)
    override func _expandAsArray_<ItemType>(_ env: Scope, itemType: ItemType) throws -> [ItemType.SwiftType]
                                            where ItemType: Coercion, ItemType: SwiftCast {
        return try self.items.map{try $0.evaluate(env, returnType: itemType)} // TO DO: FIX: this seems to be invoking Value base class methods, not Text methods
    }
    
    
    override func _expandAsList_(_ env: Scope, itemType: NativeCoercion) throws -> List {
        return List(items:  [try self.evaluate(env, returnType: itemType)], itemType: itemType)
    }
    
    override func _expandAsRecord_(_ env: Scope) throws -> Record { // TO DO: is this right? converts [1,2,3] to {1,2,3}; shouldn't it be {[1,2,3]}? A. NO, it's wrong: FIX
        return Record(try self.items.map{try $0.evaluate(env, returnType: gAnyValueCoercion)})
    }
}



class Record: Value { // roughly analogous to struct, though with different share-vs-copy semantics; note: records may be concatenated, but existing items may only have their values modified; their names are fixed and cannot be added, changed, or removed (the only way to do that is by going meta - which in turn raises question of when/where/how to do that, as a 'compiled' distributable script should generally be sealed against that sort of thing as self-modifying code, while powerful, carries risks too)
    let fields: [Value]
    
    init(_ fields: [Value]) { // TO DO: dual list+dict storage for faster index+name lookups // TO DO: enforce name:value pairs/values (simplest probably to map all unnamed fields to pairs with empty names)
        self.fields = fields
    }
    
    convenience init(_ fields: Value...) { // TO DO: dual list+dict storage for faster index+name lookups // TO DO: enforce name:value pairs/values (simplest probably to map all unnamed fields to pairs with empty names)
        self.init(fields)
    }
    
    // literal representations
    
    override var description: String { return "{" + fields.map{$0.description}.joined(separator: ", ") + "}" }
    override var debugDescription: String { return "Record(" + fields.map{$0.debugDescription}.joined(separator: ", ") + ")" }
    
    // special conversion, given a record, convert it to a RecordSignature (assuming all its fields contain Name:Value pairs where the value evals to a Coercion)
    
    func toRecordSignature() throws -> RecordSignature { // result is a record whose fields are `Name:Coercion` pairs // TO DO: this is no good, needs env
        var fields = [FieldSignature]()
        for (i, field) in self.fields.enumerated() {
            if let pair = field as? Pair { // must be literal pair, so no eval
                let fieldType = try pair.toFieldSignature() // TO DO: implement toFieldSignature on Name, returning name:AnyValueCoercion?
                // TO DO: icky coercions; generic Pair would avoid these
                fields.append(fieldType)
            } else if let name = field as? Name { // or literal name, so again no eval
                fields.append(FieldSignature(name, gAnyValueCoercion))
            } else {
                throw BadArgument(description: "Field \(i+1) of record does not have a valid name: \(field)")
            }
        }
        return RecordSignature(fields: fields)
    }
    
    // evaluation
    override func _expandAsAny_(_ env: Scope) throws -> Value {
        return try self._expandAsRecord_(env)
    }
    
    override func _expandAsArray_<ItemType>(_ env: Scope, itemType: ItemType) throws -> [ItemType.SwiftType]
                                            where ItemType: Coercion, ItemType: SwiftCast, ItemType.SwiftType: Value { // TO DO: as with List._expandAsRecord_(), this may be a bit janky
        return try self.fields.map{try $0.evaluate(env, returnType: itemType)}
    }
    
    override func _expandAsRecord_(_ env: Scope) throws -> Record { // TO DO: define RecordCoercionProtocol for use here? // TO DO: this method is wrong:
        // note: when called in `to NAME {...} ...` operator's argument record, returnType: is ParameterTypeCoercion
        return Record(try self.fields.map{try $0.evaluate(env, returnType: gAnyValueCoercion)}) // TO DO: HACK; returnType should be some sort of PairCoercion; with the exact type specified by returnType
    }
    
    override func _expandAsCoercion_(_ env: Scope) throws -> Coercion { // note: returnType is ignored as it should always be gTypeCoercion
        fatalNotYetImplemented(self, #function) // TO DO: `VALUE as {name:type,...}` should coerce VALUE to record of specified form; basically allows much more readable shorthand for `VALUE as record {structure: {name:type,...}}`
    }
}


//**********************************************************************


// TO DO: commands could (and should) have lighter-weight structure, storing name as String and args as [Value] (record access methods could be provided via protocol extension, or by having Command subclass Record); the one caveat being that normalized names are required for binding whereas non-normalized names should be retained for display -- for now, just KISS; implement public APIs as composing Value types (as that's the abstraction that symbolic metaprogramming uses), and worry about optimized APIs/internal representations later.


class Command: Value {
    // note that while a Command value has a pair-based structure, it doesn't subclass Pair as its behavior is _not_ substitutable (i.e. if the user passes a command where a pair is expected, the user expects the command to perform the corresponding procedure which then returns a pair value, not be dismantled into a `Name:AnyOrNothing` pair itself); i.e. when deciding on whether one Value should subclass another, both API substitutability _and_ behavioral substitutability must be guaranteed (i.e. don't want to get into embarrassing situations such as NSArray vs NSMutableArray where the latter guarantees the same API as the former but does _not_ guarantee the same behavior, since an NSMutableArray's contents may change at any time, including while it's being used in code that assumes its immutability since it asked for an NSArray). 
    
    let name: Name, argument: Record, key: String
    
    // TO DO: how/where to attach postfixed `do` block (expression group)?
    
    init(name: Name, argument: Value = gEmptyRecord) { // TO DO: should `argument` be Record? (main concern is if a record value is passed as argument instead of being literal, it should be used as first argument, not the entire arg record; need to decide how best to do this)
        self.name = name
        self.key = name.string.lowercased()
        // note: we're being a bit more aggressive about pushing arg to record upon instantiation rather than consumption; this might violate KISS and reduce potential for DSLs finding their own uses for existing datatypes
        // TO DO: see also TODO in parseArgument regarding `()` and `nothing`
        self.argument = argument is Record ? argument as! Record : argument is NullValue ? gEmptyRecord : Record(argument)
    }
    
    // convenience constructors for use by Swift code
    convenience init(_ name: String) {
        self.init(name: Name(name), argument: gEmptyRecord)
    }
    convenience init(_ name: String, _ argumentFields: Value...) { // argument is zero or more pair/non-pair values to be used as record fields
        self.init(name: Name(name), argument: Record(argumentFields))
    }
    
    // literal representations
    
    override var description: String { return "\(self.name) \(self.argument)" } // note that if argument record contains a single [unnamed] numeric/quoted text value or list value (which are self-delimiting literals), it may display that value instead of the enclosing record for neatness; the default description() method should always display canonical form for reference/debugging use
    override var debugDescription: String { return "Command(\(self.name.debugDescription), \(self.argument.debugDescription))" }
    
    // conversion
    
    override func toCommand() throws -> Command { return self }

    // expansion // TO DO: what, if anything, needs to be done? (e.g. thunk)
    
    override func _expandAsAny_(_ env: Scope) throws -> Value { return self }
    override func _expandAsCommand_(_ env: Scope) throws -> Command { return self }
    override func _expandAsCoercion_(_ env: Scope) throws -> Coercion { return try self.evaluate(env, returnType: gTypeCoercion) } // hmmm…
    
    override func evaluate<ReturnType>(_ env: Scope, returnType: ReturnType) throws -> ReturnType.SwiftType
                                        where ReturnType: Coercion, ReturnType: SwiftCast {
        // TO DO: this isn't quite right, e.g. if returnType is `gCommandCoercion`, it should return self; if returnType is `ThunkCoercion`, should the entire command be deferred, including proc lookup, or should the proc be looked up now and stored as a closure that is invoked when the thunk is forced? etc.
        return try env.callProcedure(self, commandScope: env, returnType: returnType)
    }
}


//**********************************************************************


class Thunk: Value { // TO DO: memoize? (kinda depends on difference between `as lazy value` and `as expression`; if there's no memoization then there's no difference, and a new result is generated each time expand is called - which causes fun e.g. when value is `random number` command; otoh if there is memoization then the result needs to be generated and stored in thunk first time expand is called - which is also fun since side-effects mean that value may be different depending on timing of that first call; in addition, if there's memoization then a separate coercion may be needed to capture as expression [although arguably that's just a degenerate form of `as procedure`])
    
    let value: Value
    let env: Scope
    let type: Coercion // TO DO: kludgy, since we can't currently union thunked and requested Coercions, so have to apply them in sequence
    
    init(value: Value, env: Scope, type: Coercion = gAnyValueCoercion) {
        self.value = value
        self.env = env
        self.type = type
    }
    
    override var description: String {
        return "Thunk(\(self.value), \(self.type))" // TO DO: this needs to escape any `"` chars within self.string by doubling them
    }
    
    override func evaluate<ReturnType>(_ env: Scope, returnType: ReturnType) throws -> ReturnType.SwiftType
                                        where ReturnType: Coercion, ReturnType: SwiftCast {
        if returnType.defersExpansion {
            return try returnType._coerce_(self, env: self.env)
        } else {
            return try self.type.intersect(returnType, env: self.env)._coerce_(self.value, env: env)
        }
    }
}


//**********************************************************************
// expression sequences

// TO DO: how to prevent `(name:value)` being treated as `store` command? (pass flag to parseExpressionSequence; do parsefunc and script parse should pass true)

// TO DO: think the only way to dodge bullets (and keep special-case syntax rules comprehensible) with Pairs is to subclass ExpressionBlock as `ExpressionBlock` and that as `EntoliScript` for `do` blocks and scripts, and have these special-case name:value pairs as store commands (probably best that they convert them to some form of `store` commands as they're parsed [e.g. flagging/wrapping/subclassing them so that they still format as pairs]; may be an idea to define `Store` as a Value/Pair/Command subclass, although that isn't ideal); a safer option might be for ExpressionBlock to recognize and convert them itself; that should avoid any awkward metaprogramming surprises (although one can argue that the formatter can and should make the decision on when to display a `store` command using name:value syntax instead, as much like operators it's acting here as syntactic sugar for the underlying command+proc)

class ExpressionBlock: Value { // TO DO: can/should these be omitted where not needed? also, need to think carefully about allowing multiple expressions (punctuation-based grouping is kinda needed as `do...done` blocks are only available if that operator is loaded); one possibility is for expression groups to coerce differently depending on whether they contain one or arbitrary no. of expressions; thus a math operator would ask for `number` and would throw coercion error if passed `(do evil. 2)`, and only asking for commands would accept it (e.g. `if{bool,commands}`)
    
    enum Format {
        case sentence
        case parenthesis
        case block
        case script
    }
    
    let expressions: [Value] // note: values are commands
    let format: Format
    
    init(expressions: [Value], format: Format) {
        self.expressions = expressions
        self.format = format
    }
    
    // literal representations
    
    override var description: String {
        switch self.format {
        case .sentence:
            return expressions.map{$0.description}.joined(separator: ", ") + ". "
        case .parenthesis:
            return "(" + expressions.map{$0.description}.joined(separator: ". ") + ")" // TO DO: what separator? (this ties in with question of whether or not parens should be grouping only, e.g. around a single operator expression, or whether they should be able to wrap a sequence of expressions)
        case .block:
            return "do\n" + expressions.map{$0.description}.joined(separator: "\n\t") + "\ndone" // TO DO: this really needs external formatter to ensure correct indentation and trailing line break
        case .script:
            return expressions.map{$0.description}.joined(separator: "\n\n")
        }
    }
    
    // conversion
    
    override func toCommand() throws -> Command {
        if self.expressions.count != 1 { throw CastError(value: self, type: Command.self) }
        let expr = try self.expressions[0].toCommand()
        expr.annotations.append("Format.Parenthesized")
        return expr
    }
    
    // expansion
    
    // TO DO: _expandAs...()? (FWIW, bear in mind that `as expression` is basically just casting to a non-memoizing thunk [as opposed to `as lazy` which casts to memoizing thunk])
    
    // one possibility when deciding how to eval `Name:Value` pairs it to pass a flag that is initially set to `.CommandContext` (e.g. in proc body) which then flips to `.ValueContext` once it goes inside, say, a list or record or command argument (where the Name should remain a Name and only the Value evaluated, rather than the whole thing being converted to a `store` command [ahh, the joys of making side-effectful calls in a list or other collection-style data structure... always a code smell; rarely [safely] blockable])
    
    // TO DO: update following implementation (needs to account for deferred eval)
    
    // TO DO: this should cast to command before evaling so that pair values and name values are converted to corresponding `store` and `NAME(no-value)` commands; all other values should cast to `_output_{value}` (actually, simplest way to do this is to use an enum); it's important to bear in mind that new types may be defined which also act like commands, thus the cast is necessary to ensure compatibility with those
    
    // TO DO: another possibility is to define _evaluateAsProcedureCall_ (_callAsProcedure_? _call_?) on all Values, which this method can then call, providing the necessary clue about how to deal with pairs and other non-commands; unlike _expandAsCommand_ (which is really only useful for coercing Name to Command, and throwing error if it's anything else... although strictly speaking an expression group should maybe also coerce to Command, as it's just a collection of the things and should be more or less transparent from runtime's POV, c.f. kiwi's composite commands)
    
    override func evaluate<ReturnType>(_ env: Scope, returnType: ReturnType) throws -> ReturnType.SwiftType
                                        where ReturnType: Coercion, ReturnType: SwiftCast {
        // TO DO: how should returnType.defersExpansion be handled? should it apply to entire expression, or just to final result? (bear in mind that some coercions, e.g. `as expression`, need to do non-standard processing of values; it may be that `as` operator should check RH operand's Coercion.defersExpansion itself before doing anything with LH operand); for now, probably just do the simplest thing that gets stuff working
        var result: Value = gNullValue
        for expression in self.expressions {
            print("EVAL EXPR: \(expression)")
            do {
                result = try expression.evaluate(env, returnType: gAnythingCoercion) // TO DO: what type? also, how to break out of loop, e.g. when returning result? (and what, if anything, needs to be done wrt error handling, especially if error is capturing scope plus remaining commands as continuation, allowing it to resume from where it left off) // TO DO: FIX: if expression is a Name, it needs to eval as a Command; simplest might be to separate `expand` and `perform` into separate methods; also need to decide how Pair.evaluate() needs to work
            } catch {
                throw EvaluationError(description: "Can't evaluate the following expression:\n\n\t\(expression)\n\n\(error)")
            }
            print("RESULT: \(result)")
        }
        return try returnType._coerce_(result, env: env) // TO DO: returnType needs to be provided to last expression
    }
}




class EntoliScript: ExpressionBlock { // TO DO: rename `Script[Object]`?
    
    init(expressions: [Value]) {
        super.init(expressions: expressions, format: .script)
    }
    
    override var description: String {
        return self.expressions.map({$0.description}).joined(separator: "\n")
    }
    
    override var debugDescription: String {
        return "EntoliScript(\(self.expressions))"
    }
    // TO DO: need to think about persistent state, and when Script should create and maintain its own Scope instance, even across multiple calls, versus using a Scope supplied to it (e.g. in REPL-style use); the latter use case is already covered by inherited `evaluate` method; the former should also be implemented, though preferably in a way that avoids confusion on how correctly to use it (e.g. OSA API suffers from lack of clarity here), and also avoids unnecessary overhead of initializing a new env every time (note: builtin env should be inited once and write-locked, allowing it to be shared by all scripts - the rule being that the builtin env contains no native/primitive state; one might even argue for providing a separate lightweight per-script storage that any module could use rather than modifying its scope... though one could argue that if module loader wants to cache loaded modules for efficient reuse it should just read them into cache first then vend a cloned instance; much will come down to questions of e.g. script localization preferences)
}


//**********************************************************************
// convenience


let gEmptyText = Text("")
let gEmptyList = List()
let gEmptyRecord = Record()


