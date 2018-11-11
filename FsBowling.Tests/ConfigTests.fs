module FsBowling.ConfigTests

open FSharpPlus.Data
open Expecto

[<Tests>]
let tests =
    testList "Config tests" [
        test "The default config has a valid number of pins" {
            Expect.isGreaterThan (Config.defaultConfig |> Config.numberOfPins) 0 "The default config doesn't have a valid number of pins."
        }

        test "The default config has a valid number of frames" {
            Expect.isGreaterThan (Config.defaultConfig |> Config.numberOfFrames) 0 "The default config doesn't have a valid number of frames."
        }

        test "The default config has a valid max name length" {
            match Config.defaultConfig |> Config.maxNameLength with
            | Some length -> Expect.isGreaterThan length 0 "The default config doesn't have a valid max name length."
            | None -> ()
        }

        test "The default config has a valid max player count" {
            match Config.defaultConfig |> Config.maxPlayerCount with
            | Some count -> Expect.isGreaterThan count 0 "The default config doesn't have a valid max player count."
            | None -> ()
        }

        testProperty "A config can be created only with valid values" <| fun numPins numFrames maxNameLength maxPlayerCount ->
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
                (not isNumPinsValid => (errors |> List.contains (InvalidNumberOfPins numPins))) &&
                (not isNumFramesValid => (errors |> List.contains (InvalidNumberOfFrames numFrames))) &&
                (not isMaxNameLengthValid =>
                    (maxNameLength
                    |> Option.map InvalidMaxNameLength
                    |> Option.map (fun error -> errors |> List.contains error)
                    |> Option.defaultValue false)) &&
                (not isMaxPlayerCountValid =>
                    (maxPlayerCount
                    |> Option.map InvalidMaxPlayerCount
                    |> Option.map (fun error -> errors |> List.contains error)
                    |> Option.defaultValue false))
    ]
