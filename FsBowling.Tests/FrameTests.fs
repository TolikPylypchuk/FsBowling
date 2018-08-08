module FsBowling.FrameTests

open Chessie.ErrorHandling

open Xunit
open FsUnit.Xunit

[<Theory>]
[<RangeData(1, 10)>]
let ``A normal frame should be created successfully when its number is valid`` number =
    Frame.create number |> shouldBeSuccess (NormalFrame.NotStarted |> Frame.Normal)
    
[<Fact>]
let ``A last frame should be created successfully when its number is valid`` () =
    Frame.create Frame.lastFrameNumber |> shouldBeSuccess (LastFrame.NotStarted |> Frame.Last)
    
[<Theory>]
[<InlineData(0)>]
[<InlineData(-1)>]
[<InlineData(11)>]
[<InlineData(12)>]
let ``A frame should not be created successfully when its number is not valid`` number =
    Frame.create number |> shouldBeFailure [ InvalidFrameNumber number ]
