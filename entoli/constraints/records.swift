//
//  constraints/record.swift
//  entoli
//
//

// Q. given that generics don't [yet] support varargs, how to specify field types when unpacking to Swift? One option might be linked-list recursion, where each field is `FieldType<Constraint,FieldType<Constraint,...>>` -- assuming, of course, that can be terminated (plus it won't do the Swift call stack any favors either), and that Swift is any better at recursive types than varargs (and strongly suspect it isn't). It may be case that there's no good way to do this automatically for now, in which case primitive funcs will just have to extract record items directly via generic method that throws if runtime type mismatch.


//**********************************************************************


// TO DO: RecordConstraint; this needs to take `fieldTypes:[Constraint]` arg,
