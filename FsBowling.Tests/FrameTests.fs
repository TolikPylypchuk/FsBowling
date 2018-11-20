module FsBowling.FrameTests

open Expecto

[<Tests>]
let tests =
    testList "Frame tests" [
        testProperty "A frame can be created with and only with a valid frame number" <| fun configParams num ->
            (createConfig configParams) >>==> fun config ->
                match Frame.create num |> with' config with
                | Ok frame ->
                    num > 0 &&
                    num <= (config |> Config.numberOfFrames) &&
                    (frame |> Frame.number) = num &&
                    (frame |> Frame.state) = FrameState.NotStarted
                | Error error ->
                    error = InvalidFrameNumber num
    ]
