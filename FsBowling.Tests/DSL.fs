[<AutoOpen>]
module FsBowling.DSL

open FSharpPlus
open FSharpPlus.Data

let with' config reader =
    Reader.run reader config

let createConfig (numPins, numFrames, nameLength, playerCount) =
    Config.create numPins numFrames nameLength playerCount

let createPlayerName = PlayerName.create

let normalizeState state =
    Reader.ask |>> Config.numberOfPins |>> (fun numPins ->
        let halfPins = numPins / 2
        let maxPins = numPins + 1

        match state with
        | InProgress score -> InProgress (abs score % numPins)
        | Open (score1, score2) -> Open (abs score1 % halfPins, abs score2 % halfPins)
        | LastStrikeInProgress2 score -> LastStrikeInProgress2 (abs score % maxPins)
        | LastStrike (score1, score2) -> Open (abs score1 % maxPins, abs score2 % maxPins)
        | Spare score -> Spare (abs score % numPins)
        | LastSpareInProgress score -> LastSpareInProgress (abs score % numPins)
        | LastSpare (score1, score2) -> LastSpare (abs score1 % numPins, abs score2 % maxPins)
        | _ -> state)

let createFrame (num, state) = monad {
    let! config = Reader.ask
    let numPins = config |> Config.numberOfPins
    let numFrames = config |> Config.numberOfFrames
    let dummy = InvalidScore 0

    return monad {
        let! frame = Frame.create num |> with' config
        let roll score frame = Frame.roll score frame |> with' config

        return!
            match state |> normalizeState |> with' config with
            | NotStarted ->
                frame |> Ok
            | InProgress score ->
                frame |> roll score
            | Open (score1, score2) ->
                frame |> roll score1 >>= roll score2
            | Strike ->
                frame |> roll numPins
            | LastStrikeInProgress1 when num = numFrames ->
                frame |> roll numPins
            | LastStrikeInProgress2 score when num = numFrames ->
                frame |> roll numPins >>= roll score
            | LastStrike (score1, score2) when num = numFrames ->
                frame |> roll numPins >>= roll score1 >>= roll score2
            | Spare score ->
                frame |> roll score >>= roll (numPins - score)
            | LastSpareInProgress score when num = numFrames ->
                frame |> roll score >>= roll (numPins - score)
            | LastSpare (score1, score2) when num = numFrames ->
                frame |> roll score1 >>= roll (numPins - score1) >>= roll score2
            | _ -> dummy |> Error
    }
}

let (>==>) result test =
    match result with
    | Ok result -> test result
    | Error _ -> true

let (>>==>) result test =
    match result with
    | Success result -> test result
    | Failure _ -> true
