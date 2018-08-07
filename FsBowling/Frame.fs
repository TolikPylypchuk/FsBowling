namespace FsBowling

open Chessie.ErrorHandling

type NormalFrame =
    | NotStarted
    | InProgress of int
    | Open of int * int
    | Strike
    | Spare of int
    
type LastFrame =
    | NotStarted
    | InProgress of int
    | Open of int * int
    | StrikeInProgress1
    | StrikeInProgress2 of int
    | Strike of int * int
    | SpareInProgress of int
    | Spare of int * int

type Frame =
    | Normal of NormalFrame
    | Last of LastFrame

module Frame =

    let lastFrameNumber = 10

    let create num =
        if num > 0 && num < lastFrameNumber then
            NormalFrame.NotStarted |> Frame.Normal |> ok
        elif num = lastFrameNumber then
            LastFrame.NotStarted |> Frame.Last |> ok
        else
            num |> InvalidFrameNumber |> fail
