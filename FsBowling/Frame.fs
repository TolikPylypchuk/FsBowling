namespace FsBowling

open Chessie.ErrorHandling

type FrameState =
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

type Frame = {
    State : FrameState
    Number : int
}

type FrameScore = {
    Total : int option
    FirstRoll : int option
    SecondRoll : int option
    ThirdRoll : int option
}

module Frame =

    let lastFrameNumber = 10
    let numberOfPins = 10

    let private frameScore = {
        Total = None
        FirstRoll = None
        SecondRoll = None
        ThirdRoll = None
    }

    let create num =
        if num > 0 && num <= lastFrameNumber
        then { State = NotStarted; Number = num } |> ok
        else num |> InvalidFrameNumber |> fail

    let isFinished frame =
        match frame.State with
        | Open _ | Strike | Spare _ | LastStrike _ | LastSpare _ -> true
        | _ -> false

    let isLast frame =
        frame.Number = lastFrameNumber

    let private rollForNotStarted score frame =
        if score < numberOfPins then
            { frame with State = InProgress score }
        else
            if frame.Number <> lastFrameNumber
            then { frame with State = Strike }
            else { frame with State = LastStrikeInProgress1 }
            
    let private rollForInProgress firstScore secondScore frame =
        if (firstScore + secondScore) < numberOfPins then
            { frame with State = Open (firstScore, secondScore) }
        else
            if frame.Number <> lastFrameNumber
            then { frame with State = Spare firstScore }
            else { frame with State = LastSpareInProgress firstScore }

    let private rollForFinished score frame =
        { State = InProgress score; Number = frame.Number + 1 }

    let private rollForLastStrikeInProgress1 score frame =
        { frame with State = LastStrikeInProgress2 score }
        
    let private rollForLastStrikeInProgress2 firstScore secondScore frame =
        { frame with State = LastStrike (firstScore, secondScore) }
        
    let private rollForLastSpareInProgress firstScore secondScore frame =
        { frame with State = LastSpare (firstScore, secondScore) }

    let roll score frame =
        if score >= 0 && score <= numberOfPins then
            let rollFun =
                match frame.State with
                | NotStarted ->
                    rollForNotStarted |> ok
                | InProgress firstScore ->
                    let totalScore = score + firstScore
                    if totalScore <= numberOfPins
                    then rollForInProgress firstScore |> ok
                    else InvalidScore totalScore |> fail
                | Open _ when frame.Number < lastFrameNumber ->
                    rollForFinished |> ok
                | Strike | Spare _ ->
                    rollForFinished |> ok
                | LastStrikeInProgress1 ->
                    rollForLastStrikeInProgress1 |> ok
                | LastStrikeInProgress2 score ->
                    rollForLastStrikeInProgress2 score |> ok
                | LastSpareInProgress firstScore ->
                    rollForLastSpareInProgress firstScore |> ok
                | _ ->
                    RollAfterLastFrame |> fail

            rollFun |> Trial.map (fun rollFun -> rollFun score frame)
        else
            InvalidScore score |> fail

    let getScores frames =
        frames |> Seq.collect (fun frame -> seq {
            yield!
                match frame.State with
                | NotStarted ->
                    [ None; None ]
                | InProgress score ->
                    [ Some score; None ]
                | Open (firstScore, secondScore) ->
                    [ Some firstScore; Some secondScore ]
                | Strike ->
                    [ Some numberOfPins; None ]
                | LastStrikeInProgress1 ->
                    [ Some numberOfPins; None; None ]
                | LastStrikeInProgress2 score ->
                    [ Some numberOfPins; Some score; None ]
                | LastStrike (firstScore, secondScore) ->
                    [ Some numberOfPins; Some firstScore; Some secondScore ]
                | Spare score ->
                    [ Some score; Some (numberOfPins - score) ]
                | LastSpareInProgress score ->
                    [ Some score; Some (numberOfPins - score) ]
                | LastSpare (firstScore, secondScore) ->
                    [ Some firstScore; Some (numberOfPins - firstScore); Some secondScore ]
        })
        
    let getTotal =
        List.tryHead >> Option.map (fun score -> score.Total |> Option.defaultValue 0) >> Option.defaultValue 0

    let getTotalScores frames =
        let rec getTotalScores' (frameScores : FrameScore list) frames =
            match frames with
            | [] -> frameScores |> List.rev
            | frame :: otherFrames ->
                let scores = otherFrames |> getScores |> Seq.choose id
                let total = frameScores |> getTotal
                let score =
                    match frame.State with
                    | NotStarted ->
                        frameScore
                    | InProgress score ->
                        { frameScore with Total = total + score |> Some; FirstRoll = score |> Some }
                    | Open (firstScore, secondScore) ->
                        { frameScore with
                            Total = total + firstScore + secondScore |> Some
                            FirstRoll = firstScore |> Some; SecondRoll = secondScore |> Some }
                    | Strike ->
                        { frameScore with
                            Total = total + numberOfPins + (scores |> Seq.truncate 2 |> Seq.fold (+) 0) |> Some
                            FirstRoll = numberOfPins |> Some }
                    | Spare score ->
                        { frameScore with
                            Total = total + numberOfPins + (scores |> Seq.tryHead |> Option.defaultValue 0) |> Some
                            FirstRoll = score |> Some
                            SecondRoll = numberOfPins - score |> Some }
                    | LastStrikeInProgress1 ->
                        { frameScore with
                            Total = total + numberOfPins |> Some
                            FirstRoll = numberOfPins |> Some }
                    | LastStrikeInProgress2 score ->
                        { frameScore with
                            Total = total + numberOfPins + score |> Some
                            FirstRoll = numberOfPins |> Some
                            SecondRoll = score |> Some }
                    | LastStrike (firstScore, secondScore) ->
                        { frameScore with
                            Total = total + numberOfPins + firstScore + secondScore |> Some
                            FirstRoll = numberOfPins |> Some
                            SecondRoll = firstScore |> Some
                            ThirdRoll = secondScore |> Some }
                    | LastSpareInProgress score ->
                        { frameScore with
                            Total = total + numberOfPins |> Some
                            FirstRoll = score |> Some
                            SecondRoll = numberOfPins - score |> Some }
                    | LastSpare (firstScore, secondScore) ->
                        { frameScore with
                            Total = total + numberOfPins + secondScore |> Some
                            FirstRoll = firstScore |> Some
                            SecondRoll = numberOfPins - firstScore |> Some
                            ThirdRoll = secondScore |> Some }

                otherFrames |> getTotalScores' (score :: frameScores)

        frames |> getTotalScores' []
