//
//  values/base.swift
//  entoli
//
//
//

// expand vs perform; all objects should implement both; the difference is that `perform` acts as command evaluator whereas `expand` is value evaluator (Q. how should a command behave? it's a first-class object, so could expand to itself, as opposed to call proc); another option is to pass flag to evaluate indicating whether to .expand, .perform, or .preferred... Q. is there any value [sic] in `expand`, given that entoli != kiwi, so doesn't have expandable tags - so what's left? Name -> Name or Command; any others?


import Foundation

// TO DO: what about implementing `evaluate()->Value` funcs as dynamic alternative to the static `evaluate<ReturnType>()->ReturnType.SwiftType`?

// TO DO: what about implementing NativeConstraint protocol that can be used as parameter type instead of `Constraint` (which can't coerce) and `SwiftConstraint` (which requires generics)


// TO DO: if/when Swift supports `override` in extensions, move cast and evaluation methods to extensions

// TO DO: should `description` return entoli literal representation and `debugDescription` return Swift literal representation? (e.g. would make error messages easier to build)

// note: KISS; standard AST-walking eval doesn't need to be (and shouldn't be) optimized, and consistency+absence of 'shortcuts' will avoid creating hidden behavioral 'holes' that tools such as interactive viewers and code generators can't analyse (e.g. expression group should always coerce items to commands, even though it's not very efficient, as that method call can be sniffed by value-like objects as well as values)

// `foo as anything` calls `foo` proc and returns result; `foo as expression` returns `foo` command; `foo as procedure` looks up `foo` name and returns closure

// TO DO: pretty sure we need `NAME as text` and `TEXT as name` casts (though need to be careful about not applying them as coercions anywhere the distinction matters, e.g. "foo" in a command context should eval as itself, not call `foo` proc)

// also need to think about how AOT compilation might change code behavior (since all movements from entoli to Swift datatypes will require primitive coercions to specific types, where previously entoli code might accept any type)

// TO DO: how best to support selective stepped eval (e.g. step by command/block); simplest would be to transform code before execution (e.g. using SteppedExpressionSequence in place of ExpressionBlock), rather than passing flags which ExpressionBlock must always check


// note: value-to-value conversion methods have `as` prefix and should be supported on all objects (throwing a ConstraintError if not appropriate)
// methods to convert standard types to special types (including non-values) have `to` prefix, e.g. `toScalar`; these appear only on objects to which they apply

// TO DO: all values should have a slot containing Constraint object describing their canonical type (e.g. Text contains TextConstraint, List contains ListConstraint(gAnyValueConstraint)); question is what to do when more than one is acquired, e.g. as value passes thru additional coercions - might want to keep all such coercions in an internal Set so that subsequent passes of that value require only a `contains` test, while allowing the best type to be calculated lazily (e.g. whole number) if/when that first check doesn't give a match

    // TO DO: a named pair _must_ evaluate as `store`, except in list (where both operands are evaled as exprs and a key-value pair returned), record (remains as a name-value pair, with eval of right operand determined by arg/expr context), or [parensed?] pair value (remains as pair, with coercion context determining eval)

// TO DO: implement ExpressibleByStringLiteral, etc.

// TO DO: might be an idea to move _expandAsTYPE_ methods to extensions in a separate file for easier maintenance



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
    // note: _expandAs... methods are used by Constraints to convert between standard entoli Values (text,name,command,pair,list,record), resolving any unevaluated expressions as necessary
    // TO DO: rather than throw, should most/all of these call _expandAsType_ catchall, which can then throw ConstraintError by default; this would allow e.g. GroupExpression to override _expandAsType_() rather than evaluate(), which might be safer
    
    func _expandAsAny_(_ env: Scope) throws -> Value { // subclasses must override, e.g. to call their corresponding _expandAsTYPE_()
        fatalNotYetImplemented(self, #function)
    }
    
    // TO DO: all these methods should bounce off a [generic?] catch-all method
    
    func _expandAsText_(_ env: Scope) throws -> Text {
        throw ExpansionError.unsupportedType(self, #function)
    }
    func _expandAsName_(_ env: Scope) throws -> Name {
        throw ExpansionError.unsupportedType(self, #function)
    }
    // TO DO: need _expandAsTuplePair_, and implement _expandAsPair_ based on that
    func _expandAsPair_<KeyType, ValueType>(_ env: Scope, keyType: KeyType, valueType: ValueType) throws -> Pair
                                            where KeyType: Constraint, KeyType: SwiftConstraint, KeyType.SwiftType: Value,
                                                ValueType: Constraint, ValueType: SwiftConstraint, ValueType.SwiftType: Value {
        throw ExpansionError.unsupportedType(self, #function)
    }
    func _expandAsArray_<ItemType>(_ env: Scope, itemType: ItemType) throws -> [ItemType.SwiftType]
                                    where ItemType: Constraint, ItemType: SwiftConstraint {
        return [try self.evaluate(env, returnType: itemType)]
    }
    func _expandAsList_(_ env: Scope, itemType: NativeConstraint) throws -> List {
        return List(items:  [try self.evaluate(env, returnType: itemType)], itemType: itemType)
    }
    
    // TO DO: what about expanding record as tuple/struct/class without unnecessary reboxing/unboxing?
    
    func _expandAsRecord_(_ env: Scope) throws -> Record { // need to pass fieldTypes or similar if expanding here
        // TO DO: what about passing `type:RecordSignature?` or is it simpler to convert to record first, then call Record.toRecordSignature to map fields to template? (since no. of fields are small, it probably doesn't make all that much difference); note: main problem with passing signature is that all fields need to be native types
        return Record(self) // TO DO: self should be expanded with first field type
    }
    func _expandAsCommand_(_ env: Scope) throws -> Command { // names and commands can coerce to commands (note: while named pairs are cast to 'store' commands in certain evaluation scopes, this is parser's job, not runtime's, so Pair does not implement _expandAsCommand_)
        throw ExpansionError.unsupportedType(self, #function)
    }
    
    func _expandAsConstraint_(_ env: Scope) throws -> Constraint {
        throw ExpansionError.unsupportedType(self, #function)
    }
    
    // Value.evaluate() is the standard entry point for evaluating any given value // TO DO: rename `evaluate(as returnType: ReturnType, in env: Scope)`?
    // TO DO: evaluate methods should check if value already contains cached representation for the given returnType
    // TO DO: evaluate methods should trap coercion sub-errors and rethrow as permanent ConstraintError (Q. should they also rethrow chained ConstraintErrors to provide full trace to failed item, e.g. in lists and records?)
    func evaluate<ReturnType>(_ env: Scope, returnType: ReturnType) throws -> ReturnType.SwiftType where ReturnType: SwiftConstraint {
        return try returnType.unpack(self, env: env)
    }
    func evaluate(_ env: Scope, returnType: NativeConstraint) throws -> Value {
        return try returnType.coerce(self, env: env)
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
            return "do" + expressions.map{"\n\t\($0.description)"}.joined(separator: "") + "\ndone" // TO DO: this really needs external formatter to ensure correct indentation and trailing line break
        case .script:
            return expressions.map{$0.description}.joined(separator: "\n\n")
        }
    }
    
    override var debugDescription: String {
        return "ExpressionBlock(expressions: \(self.expressions), format: \(self.format))"
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
                                        where ReturnType: Constraint, ReturnType: SwiftConstraint {
        // TO DO: how should returnType.defersExpansion be handled? should it apply to entire expression, or just to final result? (bear in mind that some coercions, e.g. `as expression`, need to do non-standard processing of values; it may be that `as` operator should check RH operand's Constraint.defersExpansion itself before doing anything with LH operand); for now, probably just do the simplest thing that gets stuff working
        var result: Value = gNullValue
        for expression in self.expressions {
            print("EVAL EXPR: \(expression)")
            do {
                result = try expression.evaluate(env, returnType: gAnythingConstraint) // TO DO: what type? also, how to break out of loop, e.g. when returning result? (and what, if anything, needs to be done wrt error handling, especially if error is capturing scope plus remaining commands as continuation, allowing it to resume from where it left off) // TO DO: FIX: if expression is a Name, it needs to eval as a Command; simplest might be to separate `expand` and `perform` into separate methods; also need to decide how Pair.evaluate() needs to work
            } catch {
                throw EvaluationError(description: "Can't evaluate the following expression:\n\n\t\(expression)\n\n\(error)")
            }
            print("RESULT: \(result)")
        }
        return try returnType.unpack(result, env: env) // TO DO: returnType needs to be provided to last expression
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

