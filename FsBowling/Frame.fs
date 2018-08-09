namespace FsBowling

open Chessie.ErrorHandling

type Frame =
    | NotStarted
    | InProgress of int
    | Open of int * int
    | Strike
    | LastStrikeInProgress1
    | LastStrikeInProgress2 of int
    | LastStrike of int * int
    | Spare of int
    | LastSpareInProgress of int
    | LastSpare of int * int

module Frame =

    let lastFrameNumber = 10
    let numberOfPins = 10

    let create num =
        if num > 0 && num < lastFrameNumber then
            NotStarted |> ok
        elif num = lastFrameNumber then
            NotStarted |> ok
        else
            num |> InvalidFrameNumber |> fail

    let isFinished frame =
        match frame with
        | Open _ | Strike | Spare _ | LastStrike _ | LastSpare _ -> true
        | _ -> false

    let private rollForNotStarted score frameNumber =
        if score > 0 && score < numberOfPins then
            InProgress score |> ok
        elif score = numberOfPins then
            if frameNumber <> lastFrameNumber
            then Strike |> ok
            else LastStrikeInProgress1 |> ok
        else
            InvalidNumberOfPinnes score |> fail
            
    let private rollForInProgress firstScore secondScore frameNumber =
        let score = firstScore + secondScore
        if score > 0 && score < numberOfPins then
            Open (firstScore, secondScore) |> ok
        elif score = numberOfPins then
            if frameNumber <> lastFrameNumber
            then Spare firstScore |> ok
            else LastSpareInProgress firstScore |> ok
        else
            InvalidNumberOfPinnes score |> fail

    let roll score frameNumber frame =
        match frame with
        | NotStarted -> rollForNotStarted score frameNumber
        | InProgress firstScore -> rollForInProgress firstScore score frameNumber
        | _ -> failwith "to do"
