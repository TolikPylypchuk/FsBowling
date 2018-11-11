[<AutoOpen>]
module FsBowling.Operators

let inline (=>) p q = (not p) || q

let inline (<=>) p q = (p => q) && (q => p)
