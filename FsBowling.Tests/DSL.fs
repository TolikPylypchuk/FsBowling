[<AutoOpen>]
module FsBowling.DSL

open Chessie.ErrorHandling

open Xunit
open FsUnit.Xunit

let failTest message = Assert.True(false, message)

let shouldBeSuccess expected =
    function
    | Ok (actual, _) -> actual |> should equal expected
    | Bad errors -> sprintf "Unexpected errors: %A" errors |> failTest
    
let shouldBeFailure expected =
    function
    | Ok (actual, _) -> sprintf "Unexpected value: %A" actual |> failTest
    | Bad errors -> errors |> should equal expected
