module FsBowling.FrameTests

open Xunit

[<Theory>]
[<RangeData(1, 11)>]
let ``A normal frame should be created successfully when its number is valid`` number =
    Frame.create number |> shouldBeSuccess { State = NotStarted; Number = number }

[<Theory>]
[<InlineData(0)>]
[<InlineData(-1)>]
[<InlineData(11)>]
[<InlineData(12)>]
let ``A frame should not be created successfully when its number is not valid`` number =
    Frame.create number |> shouldBeFailure [ InvalidFrameNumber number ]
