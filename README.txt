# entoli

entoli (phonetic Greek) = command, instruct


## Examples

Turtle drawing:

  Pen down, move forward 100mm, turn left 90°, move forward 5.5cm, pen up.

Notification handler:

  When alarm rings: say “It's a beautiful morning!”, get up,
  make breakfast [coffee {milk: 5ml, sugar: none}, toast];
  eat {while: reading paper}, walk the dog 1.25km.


## About

An experimental end-user language design, stealing ideas on semantic simplicity
and syntactic friendliness from Logo and AppleScript respectively, while also
[hopefully] avoiding their various pitfalls and errors.

Entoli syntax captures most of the readability of AppleScript, most of the
writeability of Logo, and should support spoken input and output as well.


## Features (implemented/partially-implemented/TBC)

* Unary commands, with white space allowed in case-insensitive names and no
  explicit separators (e.g. parentheses) needed to distinguish [most] argument
  values:

    move forward 100
    write "Hello"
    quit


* More complex arguments can be passed as records (tuple values with optional
  labels), which are automatically pattern matched against handler's expected
  parameter requirements. The command:

    ask user {"Please enter a number:", as: 1 thru 10}

  will be dispatched to the following handler:

    to ask user {prompt: text, as: type, giving up after: optional: seconds}: …

  and the `prompt`, `as`, and `giving up after` values matched, coerced, and
  bound to the new stack frame.


* Arguments are evaluated by the receiving handler, avoiding the need for
  special forms or lambdas in order to pass lazily-evaluated arguments:

    if {false, quit}

  (`if` is a command that evaluates its second `action` value only if its
  first `test` value evaluates to true.)


* English-like punctuation for separating commands:

  * commas as in-block separators and periods as block terminators:

      Foo, bar, baz.

  * semi-colons as pseudo-pipes that pass the result of one command as the first
    value to the next, reducing the need for explicit nesting when chaining
    a sequence of commands:

      Ask user {"Enter a number:"}; prefix "You chose "; write {to: screen}.


* Few data types, e.g. integer, float, unit types, text, date, etc. are all
  represented by `text` values, which are in turn automatically annotated with
  additional coercion and constraint information as they are consumed:

    42
    -3.14
    25.4mm
    "Hello, World!"
    2017-01-03


* No variables. Values are added to a scope using the `store` command:

    store {42, in: current age}

  and retrieved by a command of the given name:

    current age
    -> 42

  with a convenient `NAME:VALUE` shorthand for write-once storage (constants):

    first name: "Bob"
    last name: "Smith"


* Homoiconic syntax, with the option to inject custom operators as syntactic
  sugar for existing commands:

    4 + 2 / 3

  is sugar for:

    '+' {4, '/' {2, 3}}

  (Single-quoting a token explicitly identifies it as a name, even when
  overloaded as an operator.)

  and:

    if 2 + 2 = 4 do
      say "Reality OK."
    end else do
      say "Must be Monday.", quit.
    end

  is sugar for:

    'else' {left: if {test: '=' {'+' {2, 2}, 4}, action: (say "Reality OK.")},
            right: (say "Must be Monday.", quit {})}

  (`else` is an operator that evaluates its right operand only if the left
  operand returns `did nothing`; similar to Icon's success/failure mechanism.
  `do … end` is syntactic sugar for a block.)


* Other stuff, e.g. user documentation will be added directly to code as «...»
  annotations, and since code is data the whole lot can be introspected and
  manipulated by modern tooling (help systems, static checkers, autosuggest,
  autocomplete, autocorrect, etc). Incremental parsing should provide live
  syntax checking and auto-correct/user-prompt when errors or ambiguities are
  found. i.e. Fuzzy user input is accepted and embraced - the editor should
  assist the user in arriving at finished code that is canonically correct.


## Status

* Barely proof-of-concept; slowly progressing.

* Some parser bugs, changes, and TODOs not yet done.

* Swift-entoli bridging APIs still to be finalized; library glue generator
  to implement.

