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

        testProperty "A roll advances a frame to a new state if and only if the score is valid" <| fun configParams num state score ->
            (createConfig configParams) >>==> fun config ->
                (createFrame (num, state) |> with' config) >==> fun frame ->
                    let state = state |> normalizeState |> with' config
                    let numPins = config |> Config.numberOfPins
                    let numFrames = config |> Config.numberOfFrames

                    match frame |> Frame.roll score |> with' config with
                    | Ok frame ->
                        match state with
                        | NotStarted ->
                            frame |> Frame.number = num &&
                            frame |> Frame.state =
                                if score = numPins
                                then if num = numFrames then LastStrikeInProgress1 else Strike
                                else InProgress score
                        | InProgress score' ->
                            frame |> Frame.number = num &&
                            frame |> Frame.state =
                                if score + score' = numPins
                                then if num = numFrames then LastSpareInProgress score' else Spare score'
                                else Open (score', score)
                        | Open _ ->
                            frame |> Frame.number = num + 1 &&
                            frame |> Frame.state = InProgress score
                        | Strike | Spare _ ->
                            frame |> Frame.number = num + 1 &&
                            num < numFrames &&
                            frame |> Frame.state = InProgress score
                        | LastStrikeInProgress1 ->
                            frame |> Frame.number = num &&
                            num = numFrames &&
                            frame |> Frame.state = LastStrikeInProgress2 score
                        | LastStrikeInProgress2 score' ->
                            frame |> Frame.number = num &&
                            num = numFrames &&
                            frame |> Frame.state = LastStrike (score', score)
                        | LastSpareInProgress score' ->
                            frame |> Frame.number = num &&
                            num = numFrames &&
                            frame |> Frame.state = LastSpare (score', score)
                        | _ -> false
                    | Error error ->
                        match state with
                        | LastStrike _ | LastSpare _ ->
                            error = RollAfterLastFrame
                        | Open _ when num = numFrames ->
                            error = RollAfterLastFrame
                        | NotStarted | Open _ | Strike | Spare _ | LastStrikeInProgress1 | LastStrikeInProgress2 _ | LastSpareInProgress _ ->
                            error = InvalidScore score
                        | InProgress score' ->
                            error = InvalidScore (score' + score)
    ]
