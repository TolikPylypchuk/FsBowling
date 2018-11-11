[<AutoOpen>]
module FsBowling.Operators

let inline (=>) p q = (not p) || q
