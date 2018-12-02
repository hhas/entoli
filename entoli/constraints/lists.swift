//
//  constraints/lists.swift
//  entoli
//
//


//**********************************************************************


class ArrayConstraint<ItemConstraint: BridgingConstraint>: BridgingConstraint {
    
    typealias SwiftType = [ItemConstraint.SwiftType]
    
    let itemType: ItemConstraint
    let min: Int
    let max: Int
    
    init(itemType: ItemConstraint, min: Int = 0, max: Int = Int.max) {
        self.itemType = itemType
        self.min = min
        self.max = max
    }
    
    override func defaultValue(_ env: Scope) throws -> Value { return List() }
    
    // func _expandAsArray_<ItemType: BridgingConstraint>(_ env: Scope, itemType: ItemType) throws -> [ItemType.SwiftType]
    
    func unpack(_ value: Value, env: Scope) throws -> SwiftType { // TO DO: implement
        fatalNotYetImplemented(self, #function)
        // TO DO: use `toArray()->[Value]` instead, then expand items here?
        /*
         let result = try value._expandAsArray_(env: env, itemType: self.itemType) // compiler complains it can't infer itemType
         if result.count < self.min {
         throw ConstraintError(value: value, constraint: self, description: "Expected at least \(self.min) items but found \(result.count)") // TO DO: `@inline(__always) pluralize()` helper function
         } else if result.count > self.max {
         throw ConstraintError(value: value, constraint: self, description: "Expected at most \(self.max) items but found \(result.count)")
         }
         return result
         */
    }
}

extension ArrayConstraint { // deep-wrap rawValue array when it contains non-Value elements (this is recursive so may take some time on large collections of Swift values) // TO DO: might be worth shallow-wrapping large data structures and only wrap individual items if/when they are actually used
    
    func pack(_ rawValue: SwiftType, env: Scope) throws -> Value {
        // TO DO: need to catch and rethrow temporary errors (e.g. NullConstraintError) as permanent coercion errors; ditto elsewhere
        return List(items: try rawValue.map{try self.itemType.pack($0, env: env)}) //, itemType: self.itemType) // TO DO: need NativeConstraint for itemType parameter // TO DO: should annotated type always be converted to fully native Constraint? or can/should that be left till first time it's actually used?
    }
}

extension ArrayConstraint where ItemConstraint.SwiftType: Value { // shallow-wrap rawValue array when it contains Value elements
    
    func pack(_ rawValue: SwiftType, env: Scope) throws -> Value {
        return List(items: rawValue) //, itemType: self.itemType) // TO DO: need NativeConstraint for itemType parameter
    }
    
}


class ListConstraint: BridgingConstraint, NativeConstraint {
    
    typealias SwiftType = List
    
    let itemType: NativeConstraint
    let min: Int
    let max: Int
    
    init(itemType: NativeConstraint, min: Int = 0, max: Int = Int.max) {
        self.itemType = itemType
        self.min = min
        self.max = max
    }
    
    override func defaultValue(_ env: Scope) throws -> Value { return List() }
    
    // func _expandAsArray_<ItemType: BridgingConstraint>(_ env: Scope, itemType: ItemType) throws -> [ItemType.SwiftType]
    
    func unpack(_ value: Value, env: Scope) throws -> SwiftType { // TO DO: implement
        return try value._expandAsList_(env, itemType: self.itemType)
    }
    
    func wrap<ItemType>(_ rawValue: [ItemType], env: Scope) throws -> Value {
        fatalError("TODO: how to wrap items?")
    }
}


//**********************************************************************
//


class TuplePairConstraint<KeyConstraint: BridgingConstraint, ValueConstraint: BridgingConstraint>: BridgingConstraint {
    
    typealias SwiftType = (KeyConstraint.SwiftType, ValueConstraint.SwiftType)
    
    let left: KeyConstraint
    let right: ValueConstraint
    
    init(left: KeyConstraint, right: ValueConstraint) {
        self.left = left
        self.right = right
    }
    
    func unpack(_ value: Value, env: Scope) throws -> SwiftType { // returns a 2-item tuple
        // try value.asPair()
        fatalNotYetImplemented(self, #function) // TO DO: implement
    }
    
    func pack(_ rawValue: SwiftType, env: Scope) throws -> Value { // TO DO: is this right?
        return try Pair(self.left.pack(rawValue.0, env: env), self.right.pack(rawValue.1, env: env))
    }
    
}


class PairConstraint<KeyConstraint: BridgingConstraint, ValueConstraint: BridgingConstraint>: TuplePairConstraint<KeyConstraint, ValueConstraint>, NativeConstraint where KeyConstraint.SwiftType: Value, ValueConstraint.SwiftType: Value {
    
    typealias SwiftType = Pair
    
    func unpack(_ value: Value, env: Scope) throws -> Pair {
        return try self.pack(super.unpack(value, env: env), env: env) as! Pair
    }
    
    func coerce(_ value: Value, env: Scope) throws -> Value {
        return try self.unpack(value, env: env)
    }
}

