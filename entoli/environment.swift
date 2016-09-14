//
//  environment.swift
//  entoli-run
//
//


// note: primitive libraries should include flags indicating if they are interpreter- and thread-`clean` or not (e.g. a Swift-based library containing global vars cannot safely be used by more than one interpreter instance)


// TO DO: need to decide if procedures should have the right to inject data into an argument's [sub]scope prior to evaling it, as kiwi does; this has benefit of allowing first-class expressions to receive arguments (under pre-defined names) without the need to declare a parameter list; the potential downside is that injected names might mask identical names in value's original context (although that can still be gotten around by specifying `NAME of parent` to identify the original context first); one thing to remember is that the value's original scope needs to be passed along with it (kiwi uses a slightly different approach where added values are passed 'back' via dynamic stack)

// note: Proc objects could be captured as runtime values using `NAME as procedure` cast (note: `NAME as expression` won't do it as that captures an entire, eval-able, expression, whereas all we want here is a procedure value which we can later invoke with our own arguments; that said, assigning a procedure value to a scope slot should 'install' it as a regular, callable procedure.)

// refcycle checking for values is likely to be a pain; otoh, checking for scopes might be much easier, and code that binds scopes to values (typespecs; anything else) could return flags to indicate if/where such bindings have been created, as hint of where uncycler should look. (Note that refcycles in most values should be extremely rare, as A. it's unusual to put a list inside a list, and B. entoli is inclined to make lists immutable by default to reduce user confusion. Records might be more of an issue, since they may be cast to objects, which are just Scopes in Value wrappers [similar to AS's script objects], and object graphs can become heinously complex even in scripts; that said, entoli's preference is to use data-only records in conjunction with structural proc dispatch, and any fancy object mularkey is probably best done using library-based datatypes)

// TO DO: what are implications of storing first-class expressions in slots as raw values (as opposed to procs)? (suspect as with unbound procs, they need to be wrapped as thunks when `EXPR as expression` is applied; main issue is avoiding refcycles where possible, in which case it's probably a matter of checking if raw value's value is expr and thunking it if it is)

// Q. when delegating a call to a parent scope, need to think about difference between `continue CMD` and `CMD of parent` (currently, upcalls aren't allowed as a native proc only has access to its lexical scope, not the scope to which the command was initially sent)

//**********************************************************************


enum Slot {
    case storedValue(Value) // from an interaction POV, entoli doesn't have variables, just 'procedures' that return values previously stored under the same name; however, implementing these as actual closures would create more complexity than is justified, so we just store the value directly (i.e. fake it); only core operations such as `NAME as procedure` should need to know the difference, and ideally even that coupling should be eliminated (e.g. by implementing toClosure method on Scope)
    case unboundProcedure(Procedure) // standard procedure in the scope in which it was defined
    case encapsulatedProcedure(Closure) // closure value containing both a procedure the scope in which it was originally defined (note: primitive procedures that don't interact with their lexical scope could just use an empty Scope to reduce likelihood of refcycles, though not sure how helpful that'd be in practice given that most closures contain non-trivial - i.e. native - logic, or are only used as proc arguments and not retained beyond completion of that proc)
    // TO DO: how to represent an overloaded proc? should it be possible to define overloads in sub-frames without masking those in parents? simplest option would be to require all overloads defined in same frame, and store as UnboundProcedure(OverloadedProcedure), which provides a transparent dispatch wrapper around them all
    
    func call<ReturnType: Coercion>(_ command: Command, commandScope: Scope, procedureScope: Scope, returnType: ReturnType) throws -> ReturnType.SwiftType where ReturnType: CoercionProtocol {
        // check if slot is a pseudo-closure that simply returns a stored value (the default for user-stored values), a procedure implicitly bound to scope in which it was found (the default for procedures), or a full closure that includes its own lexical scope (used when a procedure defined in one scope is assigned to another)
        switch self {
        case .storedValue(let value):
            if command.argument.fields.count != 0 {
                throw BadArgument(description: "Unexpected argument for command: \(command)")// TO DO: need to nail down jargon: a command can have only zero or one arguments
            }
            print("GET \(command) -> \(value)")
            return try value.evaluate(procedureScope, returnType: returnType) //._coerce_(value, env: procedureScope)
        case .unboundProcedure(let procedure):
            return try procedure.call(command, returnType: returnType, commandScope: commandScope, procedureScope: procedureScope)
        case .encapsulatedProcedure(let closure):
            return try closure.procedure.call(command, returnType: returnType, commandScope: commandScope, procedureScope: closure.procedureScope)
        }
    }

}


//**********************************************************************
// closure

class Closure: Value { // basically a Value-compatible struct enclosing proc plus its original scope
    
    // note: these properties are accessed directly by Scope.call(); don't think there's any reason to implement another call() method here
    let procedure: Procedure
    let procedureScope: Scope // the procedureScope
    
    init(procedure: Procedure, procedureScope: Scope) {
        self.procedure = procedure
        self.procedureScope = procedureScope
    }
    
    // literal representations
    
    override var description: String {
        return "Closure(\(type(of: self)), \(self.procedureScope))"
    }
}


//**********************************************************************


class Scope: CustomStringConvertible {

    private typealias Frame = [String:Slot] // technically, values should always be procs, which are simple closures over value to return (in simple case) or primitive func or entoli expr+binding record (as struct? closure?) that synthesizes return value or RuntimeError, given env and command args as arguments (note: struct would be safest choice, as that's introspectable, plus capturing entoli contexts in Swift runtime will make cycle checking even tougher to do)
    
    private let parent: Scope!
    
    let reservedNames: Set<String> = ["", gNullValue.keyString] // TO DO: should this be static, and available to introspection // TO DO: should `parent` [possibly with some sort of prefix] be a reserved name that procedure() knows to recognize, allowing entoli code to refer to parent scope (note: this is probably part of larger question on how to distinguish and refer to local vs global scopes, library scopes, etc) // TO DO: should `store` be a protected name too? (need to give some thought to how name overloading/masking should be dealt with, particularly as it might affect program predictability/security/compilation, e.g. one option would be to treat collisions as errors unless explicit `override` operator is applied, and need to be particularly careful if importing procs into existing scopes c.f. `from MODULE import *`, which in entoli interpreter would probably be done using closures so would just be implemented as an ordinary command that does batch assignment from given scope to current scope)
    
    // TO DO: probably need some way to distinguish global from local scopes, e.g. when using `store` to modify global scope; or is it better to define a `script` slot on global Scope that returns self (wrapped as Value)?
    
    // TO DO: write locks; also need some sort of read locks for differentiating a module's public (exported) procs from its private procs (i.e. visibility should be a formal mechanism, rather than [e.g.] a Python-style naming convention; might even be possibility of versioned interfaces, or of introspecting a public module interface outside of normal runtime, e.g. online repository/documentation tools); what other [meta]info should be stored here?
    
    // TO DO: option for sealing, preventing new slots from being added [by callers] and locking existing slots' read/write settings; `store` calls would subsequently delegate to parent scope (assuming no write barrier inbetween); e.g. a `tell` block would lock all its slots (actually, it'd probably use a custom LazyScope subclass that only initializes slots on first use, as in normal usage the majority will never be used so initializing them all would be a waste of time)

    private var frame: Frame = [gNullValue.keyString: .storedValue(gNullValue)] // TO DO: what's best; adding special values to every frame, or defining them in a special 'builtins' scope that is automatically used as root scope when no parent is given
    
    init(parent: Scope? = nil) {
        self.parent = parent
    }
    
    var description: String { return "Scope:\(Array(self.frame.keys))" }
    
    func makeSubScope() -> Scope { // TO DO: what args? e.g. access rights; need to consider where barriers go: for modules, write barrier is on outside (i.e. the module can still manipulate its own state, though it may be preferable to lock that too once module is initialized)
        return Scope(parent: self)
    }
    
    // TO DO: will need a way to distinguish global (module) scopes from local (e.g. Bool flag/scope name arg? separate constructors?), and provide `this script` command (or hardcoded shortcut) to return it; Q. should Scope be Value subclass, or boxed? (sidenote: that takes us close to supporting prototype OOP; a proc that ends with `return {this scope}` would return its frame) note that supporting toRecord coercion would allow easy way to introspect any module or proc frame
    
    func store(_ keyString: String, slot: Slot) throws {
        // TO DO: need to check if slot is already filled, and check type, access rights, etc if it is and throw error if it can't be replaced [with new value]; also, how to persist type info if value is mutable? also, when setting a slot, how best to ensure new value is appropriate? (suggest that any replacement value must match core signature of original value [i.e. types must match, but some constraints such as numeric ranges might be ignored unless they were explicitly specified at the time the original value was bound - i.e. values need to differentiate between annotations they collect while building up information about themselves, and requirements ascribed by user in specific situations such as `store` command args, where an explicit `as` clause is intended not only to be applied to the value prior to storing it but to describe the permanent nature of that slot as well])
        // TO DO: what about masking? (except for )
        // TO DO: certain names must _never_ be masked or overwritten, so will either need to be initialized in every scope, or else checked for here
        if self.reservedNames.contains(keyString) { throw ScopeError(description: "Can't change value for \(Name(keyString)).") }
        if let existingSlot = self.frame[keyString] {
            print("overwriting slot: \(existingSlot)")
        }
        self.frame[keyString] = slot
    }
    
    // convenience shortcuts for above (since swift isn't smart enough to auto-box)
    func store(_ name: String, value: Value) throws { // TO DO: name MUST be all-lowercase; check who's calling this; may want to add `lowercaseString` to be sure, or else take Name instead
        try self.store(name, slot: value is Closure ? .encapsulatedProcedure(value as! Closure) : .storedValue(value))
    }
    func store(_ procedure: Procedure) throws { // used by procedure loader
        try self.store(procedure.keyString, slot: .unboundProcedure(procedure))
    }
    
    func store(_ name: Name) throws { // stores a "constant", i.e. a name that evaluates to itself (e.g. 'nothing' evaluates to 'nothing') // TO DO: this is problematic, and will confuse users
        try self.store(name.keyString, slot: .storedValue(name))
    }
    
    func procedure(_ name: Name) throws -> (Slot, Scope) { // walk stack until named slot is found then returns it, else throws 'not found' error
        // note: stacks will usually be quite shallow, having local [proc] scope, script [module] scope, and entoli scope [contains stdlib procs visible to main script by default]; reference-based lookups will require jumping to other scopes, e.g. `foo of library "bar"`
        let key = name.keyString
        var procedureScope: Scope! = self
        repeat {
            if let slot = procedureScope.frame[key] { return (slot, procedureScope) }
            procedureScope = procedureScope.parent
        } while procedureScope != nil
        throw NameNotFoundError(name: name, scope: self)
    }
    
    // TO DO: `closure(name:Name)throws->Closure` for use by `as procedure` coercion? note that this'd need to create swift closure for .StoredValue slots, or else Closure class needs modified to hold Value and/or Procedure (should be safe enough using swift closures tho')
    
    
    func callProcedure<ReturnType: Coercion>(_ command: Command, commandScope: Scope, returnType: ReturnType) throws -> ReturnType.SwiftType where ReturnType: CoercionProtocol {
        let (slot, procedureScope) = try self.procedure(command.name)
        // check if slot is a pseudo-closure that simply returns a stored value (the default for user-stored values), a procedure implicitly bound to scope in which it was found (the default for procedures), or a full closure that includes its own lexical scope (used when a procedure defined in one scope is assigned to another)
        return try slot.call(command, commandScope: commandScope, procedureScope: procedureScope, returnType: returnType)
    }
}

