//
//  values/command.swift
//  entoli
//
//

import Foundation


//**********************************************************************
// name

// TO DO: ideally generic procs shouldn't have to include `Constraint` requirement, only `SwiftCast`/`NativeConstraint` protocol: if a constraint needs to be stored in a property, convert it to a `NativeConstraint` first (storing it as a `Constraint` is semi-useless anyway, as it severely limits what can be done with it, and is effectively useless for tagging Values as it'll need to be converted to a native constraint before it can be used - assuming it can be easily boxed, that conversion can still be deferred at cost of an extra object alloc that can be ameliorated anyway thru caching...or just go straight to native constraint, which costs the same, where available, and treat any coercions that don't have native equivalents as opaque and have an OpaqueValue for representing them, which should do the same thing; the only issue being unpacking them again which is a pig when they're a generic type as those aren't amenable to casts where exact type isn't known)


// TO DO: LiteralName? this'd allow us to distinguish a Name that MAY BE evaled as an arg-less command, when in an applicable context (e.g. exprseq), vs a Name that MUST always eval as a name (e.g. record field name). Note that most of the fuzziness centers around LHS of Pair values. Might even be worth putting the onus on Pair.

// Q. how should expanded values indicate that they don't need expanded again? (this sort of ties in with the Name problem)

// Q. what about `MustBeName` (always evals as name) vs `MayBeName` (evals as name or command; calling context decides)? (FixedName/FlexibleName?)

// what about Name vs ExpandedName; the latter would always eval to self; the former as command unless NameConstraint is specified (in which case it expands to ExpandedName); for a record field, the parser would emit ExpandedName (it already special-cases pairs in [some] exprseqs as `store{VALUE,NAME}` so shouldn't need to worry about that, though can still convert name to expanded name to be sure); note: should `(NAME)` eval as command where NameConstraint is specified, or should user be required to write `NAME{}` to explicitly disambiguate? Could be fiddly; consider procs that take a name as parameter, and how a literal(?) name arg is best disambiguated from a command. (One could argue the need for `NAME{}` is self-evident, and parens should not change meaning of `NAME`; the flipside, of course, is where parens are used to group multiple exprs...though one could argue parens only ever group atomic values [which an exprseq is], in which case adding/removing parens should never modify meaning. In which case meaning of `NAME:VALUE` is always `store` except in value contexts, e.g. record, kv-list). Mind, to put an exprseq in a record, it _must_ be parensed [or do...done, assuming that works syntactically].


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
//  override func _expandAsAny_(_ env: Scope) throws -> Value { return try self.toCommand() } // (EXPERIMENTAL; currently doesn't work either) TO DO: returning Name here is problematic when the name appears in an expression context where it should be treated as an arg-less command. OTOH expanding Name as a Command here currently breaks parameter record parsing (it likely also breaks stuff elsewhere), though that's probably a defect in the RecordSignature constraint or similar, which should be asking for known arg types in the first place. One possibility would for `Value` to provide separate `call()` and `expand()` methods, the first of which to be called in command contexts (e.g. expression sequence, list pair), the other in value contexts (e.g. record pair, `store` pair). // CAUTION: this is likely subtly wrong: entoli should respect any valid program, even one directly constructed as data rather than parsed from code; thus is a Name:Value Pair appears in a List it should remain as-is - as an item in an ordered/unique list, not a key-value pair in a key-value list. IOW, the parser should track context.
    
    override func _expandAsName_(_ env: Scope) throws -> Name { return self }
    override func _expandAsCommand_(_ env: Scope) throws -> Command { return try self.toCommand() } // note: this doesn't capture env; will that be a problem? // TO DO: see TODO below
    override func _expandAsConstraint_(_ env: Scope) throws -> Constraint { // TO DO: is this okay, or is there any info that should be passed?
        return try self.toCommand().evaluate(env, returnType: gTypeConstraint)
    }
}


// TO DO: might be better to have LiteralName, as that's what's ambiguous until evaled, either as a Name or as an argless Command depending on context


let gNullName = Name("")


//**********************************************************************
// command


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
    override func _expandAsConstraint_(_ env: Scope) throws -> Constraint { return try self.evaluate(env, returnType: gTypeConstraint) } // hmmm…
    
    override func evaluate<ReturnType>(_ env: Scope, returnType: ReturnType) throws -> ReturnType.SwiftType
        where ReturnType: Constraint, ReturnType: SwiftCast {
            // TO DO: this isn't quite right, e.g. if returnType is `gCommandConstraint`, it should return self; if returnType is `ThunkConstraint`, should the entire command be deferred, including proc lookup, or should the proc be looked up now and stored as a closure that is invoked when the thunk is forced? etc.
            return try env.callProcedure(self, commandScope: env, returnType: returnType)
    }
}


