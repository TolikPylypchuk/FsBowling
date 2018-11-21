[<AutoOpen>]
module FsBowling.DSL

open FSharpPlus.Data

let createConfig (numPins, numFrames, nameLength, playerCount) =
    Config.create numPins numFrames nameLength playerCount

let createPlayerName = PlayerName.create

let with' config reader =
    Reader.run reader config

let (>>=>) result test =
    match result with
    | Ok result -> test result
    | Error _ -> true

let (>>==>) result test =
    match result with
    | Success result -> test result
    | Failure _ -> true
