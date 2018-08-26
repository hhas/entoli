//
//  values/lists.swift
//  entoli
//
//

import Foundation


//**********************************************************************
//


class List: Value { // TO DO: how best to constrain as array/dictionary/set? (parser hints, first usage); e.g. `[1, nothing, []]` might require an explicit `as list of anything` cast if standard behavior is to infer itemType from first item added (alternatively, it could just replace that first guess and force it to be looser as non-matching types are encountered; the question then being should only literals be able to do this, as opposed to values returned by commands or subsequently added to an editable list); conversely, `[1,2,3]` would require an explicit `as list of whole number` to restrict it beyond its automatically inferred `list of text` type
    
    // Q. to what extent should lists do copy-on-write? (note that Swift stdlib includes funcs for determining unique ownship, avoiding need for unnecessary copying); TBH, this question ties in with larger question of immutable vs mutable; another possibility is for collections to be implicitly mutable within their originating scope, then 'sealed' as immutable once they leave that scope unless explicitly declared e.g. `as mutable list`
    
    let items: [Value] // TO DO: prob needs to be enum of Array|Dictionary|Set|Empty (note: `Empty` just means List's internal representation hasn't been determined yet; that should resolve when first item is added, or when a coercion is applied, or possibly if type can be inferred from static analysis [though the first two options are simpler and likely preferred])
    
    
    // TO DO: should the original ListConstraint(...) type spec also be included? (will depend on how mutability works: if min/max are specified for editable list, these should be enforced when list changes size)
    
    private(set) var itemType: NativeConstraint? // TO DO: need to decide how/when this is set/unset/enforced; also, mutability should be part of type
    
    // TO DO: adding a named Pair to a List is problematic, as normal evaluation rules say it should eval as `store`, but that is undesirable in list; best if first item is checked to see if it's a Pair; if it is, and key is Name, store Name as dictionary key (i.e. parenthesizing name will cause it to eval as command, returning whatever value type it likes [including another Name, which is how AS-style 'constants' can be implemented, caveat the parameter-grabbing problem which isn't ideal but should always be caught at runtime as 'too many args' error, and can be ameliorated at edit-time by auto-correct parenthesizing or otherwise punctuating name based on its 'proc' definition])
    
    init(items: [Value], itemType: NativeConstraint? = nil) { // TO DO: problem here is ordered/key-value/unique aren't fully interchangeable
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
        return try self._expandAsList_(env, itemType: gAnyValueConstraint)
    }
    
    // TO DO: passing itemType as separate param is kinda smelly; would be better to have ListConstraintProtocol; alternatively, might be best to get rid of returnType: entirely and let Constraint.coerce tag the returned Value, and Value.evaluate() rethrow with full error details
    // TO DO: get rid of this and provide toArray() method instead? this'd leave calling Constraint to coerce each item, avoiding need to pass itemType (or returnType)
    override func _expandAsArray_<ItemType>(_ env: Scope, itemType: ItemType) throws -> [ItemType.SwiftType]
        where ItemType: Constraint, ItemType: SwiftConstraint {
            return try self.items.map{try $0.evaluate(env, returnType: itemType)} // TO DO: FIX: this seems to be invoking Value base class methods, not Text methods
    }
    
    
    override func _expandAsList_(_ env: Scope, itemType: NativeConstraint) throws -> List {
        return List(items:  [try self.evaluate(env, returnType: itemType)], itemType: itemType)
    }
}


let gEmptyList = List()


