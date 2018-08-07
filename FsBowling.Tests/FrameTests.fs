module FsBowling.FrameTests

open Chessie.ErrorHandling

open Xunit
open FsUnit.Xunit

[<Theory>]
[<RangeData(1, 10)>]
let ``A normal frame should be created successfully when its number is valid`` num =
    match Frame.create num with
    | Ok (frame, []) -> frame |> should equal (NormalFrame.NotStarted |> Frame.Normal)
    | _ -> sprintf "Failed creating a frame with number %i" num  |> failTest
    
[<Fact>]
let ``A last frame should be created successfully when its number is valid`` () =
    match Frame.create Frame.lastFrameNumber with
    | Ok (frame, []) -> frame |> should equal (LastFrame.NotStarted |> Frame.Last)
    | _ -> sprintf "Failed creating a frame with number %i" Frame.lastFrameNumber  |> failTest
    
[<Theory>]
[<InlineData(0)>]
[<InlineData(-1)>]
[<InlineData(11)>]
[<InlineData(12)>]
let ``A frame should not be created successfully when its number is not valid`` num =
    match Frame.create num with
    | Ok _ -> sprintf "Succeeded creating a frame with number %i" num  |> failTest
    | Bad errors ->
        match errors with
        | error :: [] -> error |> should equal (InvalidFrameNumber num)
        | _ -> sprintf "Unexpected error %A" errors |> failTest
