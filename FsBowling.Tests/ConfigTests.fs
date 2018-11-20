module FsBowling.ConfigTests

open FSharpPlus.Data
open Expecto

[<Tests>]
let tests =
    testList "Config tests" [
        testProperty "The default config has a valid number of pins" <| fun () ->
            (Config.defaultConfig |> Config.numberOfPins) > 0

        testProperty "The default config has a valid number of frames" <| fun () ->
            (Config.defaultConfig |> Config.numberOfFrames) > 0

        testProperty "The default config has a valid max name length" <| fun () ->
            match Config.defaultConfig |> Config.maxNameLength with
            | Some length -> length > 0
            | None -> true

        testProperty "The default config has a valid max player count" <| fun () ->
            match Config.defaultConfig |> Config.maxPlayerCount with
            | Some count -> count > 0
            | None -> true

        testProperty "A config can be created with and only with valid values" <| fun numPins numFrames maxNameLength maxPlayerCount ->
            let isNumPinsValid = numPins > 0
            let isNumFramesValid = numFrames > 0
            let isMaxNameLengthValid = maxNameLength |> Option.map (fun length -> length > 0) |> Option.defaultValue true
            let isMaxPlayerCountValid = maxPlayerCount |> Option.map (fun count -> count > 0) |> Option.defaultValue true

            let isConfigValid = isNumPinsValid && isNumFramesValid && isMaxNameLengthValid && isMaxPlayerCountValid

            match Config.create numPins numFrames maxNameLength maxPlayerCount with
            | Success config ->
                isConfigValid &&
                config |> Config.numberOfPins = numPins &&
                config |> Config.numberOfFrames = numFrames &&
                config |> Config.maxNameLength = maxNameLength &&
                config |> Config.maxPlayerCount = maxPlayerCount
            | Failure errors ->
                not isConfigValid &&
                (not isNumPinsValid = (errors |> List.contains (InvalidNumberOfPins numPins))) &&
                (not isNumFramesValid = (errors |> List.contains (InvalidNumberOfFrames numFrames))) &&
                (not isMaxNameLengthValid =
                    (maxNameLength
                    |> Option.map InvalidMaxNameLength
                    |> Option.map (fun error -> errors |> List.contains error)
                    |> Option.defaultValue false)) &&
                (not isMaxPlayerCountValid =
                    (maxPlayerCount
                    |> Option.map InvalidMaxPlayerCount
                    |> Option.map (fun error -> errors |> List.contains error)
                    |> Option.defaultValue false))
    ]
