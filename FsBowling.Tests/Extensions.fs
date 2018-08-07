[<AutoOpen>]
module FsBowling.Extensions

open Xunit

let failTest message = Assert.True(false, message)
