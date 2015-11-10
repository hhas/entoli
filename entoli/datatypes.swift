//
//  datatypes.swift
//  entoli
//
//

// TO DO: need to implement full annotation support

// TO DO: need to start thinking about lexical scoping and closures


class Value: CustomStringConvertible { // being homoiconic, there is no difference between a data value and an AST node; even commands and operators are values
    var annotations = [String]() // TO DO: structure, API, literalRepresentation
    var description: String {return "<VALUE>"} // TO DO: make this an up-call, and implement `literalRepresentation` methods that take additional options (e.g. always/never quote names); also implement debugDescription that returns Swift-style representation
}

class ListValue: Value { // TO DO: how best to constrain as array/dictionary/set? (parser can provide array-vs-dict hint when creating from literal; otherwise it's down to usage)
    let data: [Value]
    
    init(data: [Value]) {
        self.data = data
    }
    override var description: String {
        return "[" + data.map{$0.description}.joinWithSeparator(", ") + "]"
    }
}

class TextValue: Value { // TO DO: how to annotate with numerics, units, dates, etc? (data detectors might output primitive [Swift] values automatically, in which case they should prob. be cached in TextValue for efficient reuse, avoiding need to coerce Text to primitives each time); alternatively, TextValue _might_ be subclassed as NumberValue (which in turn might be subclassed as NumericUnitsValue), DateValue, etc., at least for built-ins
    let data: String
    
    init(data: String) {
        self.data = data
    }
    override var description: String {
        return "\"\(self.data)\"" // TO DO: this needs to escape any `"` chars within self.data by doubling them
    }
}

class NameValue: TextValue {
    override var description: String {
        return "'\(self.data)'" // TO DO: this needs to escape any `'` chars within data; in addition, quotes should be omitted if unnecessary
    }
}


class ExpressionGroupValue: Value { // TO DO: can/should these be omitted where not needed? also, need to think carefully about allowing multiple expressions (punctuation-based grouping is kinda needed as `do...done` blocks are only available if that operator is loaded); one possibility is for expression groups to coerce differently depending on whether they contain one or arbitrary no. of expressions; thus a math operator would ask for `number` and would throw coercion error if passed `(do evil. 2)`, and only asking for commands would accept it (e.g. `if{bool,commands}`)
    let data: [Value] // note: values are commands
    
    init(data: [Value]) {
        self.data = data
    }
    override var description: String {
        return "(" + data.map{$0.description}.joinWithSeparator(". ") + ")"
    }
}


class RecordValue: Value {
    let data: [Value]
    
    init(data: [Value]) { // TO DO: eventually this may use a more efficient representation internally, with all values stored in a list and named values also stored in a dict, allowing both positional and named lookups to be performed more efficiently
        self.data = data
    }
    override var description: String {
        return "{" + data.map{$0.description}.joinWithSeparator(", ") + "}"
    }
}


class PairValue: Value { // note: `A:B:C` is right-associative // TO DO: probably want different classes for key:value [key-value pair] vs name:value [named property]
    let name: Value, data: Value
    
    init(name: Value, data: Value) {
        self.name = name
        self.data = data
        super.init()
   //     print("Created \(self.dynamicType): \(self)")
    }
    override var description: String {
        return (self.data.dynamicType == PairValue.self) ? "\(self.name): (\(self.data))" : "\(self.name): \(self.data)" // clarify right-association by parenthesizing; TO DO: formatter should probably make all these decisions
    }
    // TO DO: coercing to command will need to return `store {value: VALUE, name: NAME, as: type of VALUE, editable: no}` command, though bear in mind that this should only be done in command contexts
}



// TO DO: commands could (and should) have lighter-weight structure, storing name as String and args as [Value] (record access methods could be provided via protocol extension, or by having CommandValue subclass RecordValue)
class CommandValue: PairValue { // note: operators are just syntactic sugar over commands, so always parse to CommandValue // TO DO: renderer will need to supply commands with ops table so that they can choose optimal display format for themselves (one problem: how to distinguish pre/in/post-fix ops (esp. pre-vs-post) as that really requires a different command name for each)
    convenience init(name: Value) {
        self.init(name: name, data: RecordValue(data: []))
    }
    override var description: String {
        switch self.data {
        case is NameValue, is PairValue, is CommandValue:
            return "\(self.name) {\(self.data)}"
        default:
            return "\(self.name) \(self.data)"
        }
    }
    // TO DO: add `hasPrefixData:Bool` to indicate first arg is piped via `;` from previous command (in which case we can get rid of PipeValue entirely and just make sure it casts its right operand to CommandValue before setting that flag)? or should evaluator always pass previous command's result along with flag provided by PipeValue telling command whether or not to use it as its first arg? (note: passing previous result might be obligatory if implementing Icon-style success/failure model, since [Test]Failed is just another result that needs to be passed along each command in remaining sequence until handled or returned)
    // TO DO: `postfixData` (where last arg is expression group supplied by postfix `do...done`)
}



class PipeValue: Value {// TO DO: needed? or should CommandValue have slot for this? (caution: recursively traversing linked lists of commands in order to eval them is a surefire way to blow Swift stack; a safer way would be for evaluator to start at head, and have each command return the next command to eval [on success]; that said, the simplest strategy would be to get rid of PipeValue and just set a flag on the RH command telling it to consume the current output value [which the evaluator always captures as the result of the previous command] as its first arg — this also avoids complexity/confusion of some commands being items in group array and others being their own linked lists; only thing we need to check is a piped command can never consume input that wasn't actually meant for it)
    let leftExpr: Value, rightExpr: Value // TO DO: if keeping PipeValue, use a single Array<Value>, and have parsefunc append commands to it as long as additional `;` separators are encountered
    
    init(leftExpr: Value, rightExpr: Value) {
        self.leftExpr = leftExpr
        self.rightExpr = rightExpr
    }
    override var description: String {
        return "\(self.leftExpr); \(self.rightExpr)" // TO DO: need to think about when to wrap in parens for clarity
    }
}


class ExpressionSeparator: Value { // period separator, used to separate commands; for now, preserved as a postfix no-op, though might be better if commands always rendered with a period at end // TO DO: if using `!` and `?` as expression separators that also work as behavioural modifiers, this class will need updated to support those and Parser and ExpressionGroupValue will need to preserve them as well (alternatively, they should set behavioral flags on CommandValue; need to decide what approach works best, bearing in mind that they could be applied to non-commands too [although this might be considered a no-op and auto-corrected away])
    let data: Value
    
    init(data: Value) {
        self.data = data
    }
    override var description: String {
        return "\(self.data). "
    }
}


class ItemSeparator: ExpressionSeparator { // comma separator, used to separate list/record items; ditto
    override var description: String {
        return "\(self.data), "
    }
}



class EntoliScript: ExpressionGroupValue {
    override var description: String {
        return "`" + data.map{$0.description+"."}.joinWithSeparator(" ") + "`"
    }
}



