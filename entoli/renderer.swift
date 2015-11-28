//
//  renderer.swift
//  entoli-run
//
//
//

// TO DO: how practical to implement renderer in entoli itself? (this'd have benefit of being customizable, which could be useful when integrating into editor as it allows users to customize display themselves) (note: may be case of bootstrapping in Swift, then replacing with entoli implementation later)

 // note: operators are just syntactic sugar over commands, so always parse to Command // TO DO: renderer will need to supply command values with ops table so that they can choose optimal display format for themselves

// consider 'best' representation vs local 'representation' display option, where latter takes into account locally defined ops for both display and disambiguation, whereas 'best' always uses the best-looking representation without regard to whether it'll parse in a given scope