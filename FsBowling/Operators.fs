[<AutoOpen>]
module FsBowling.Operators

open Chessie.ErrorHandling

let (>>=) result f = result |> Trial.bind f

let (>=>) f1 f2 = fun a -> f1 a >>= f2

let (<!>) = Trial.lift

let (<*>) = Trial.apply
