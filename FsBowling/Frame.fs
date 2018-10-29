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
    Total : int
    FirstRoll : int
    SecondRoll : int
    ThirdRoll : int
}

module Frame =

    let lastFrameNumber = 10
    let numberOfPins = 10

    let private frameScore = {
        Total = 0
        FirstRoll = 0
        SecondRoll = 0
        ThirdRoll = -1
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
        if score >= 0 && score < numberOfPins then
            { frame with State = InProgress score }
        else
            if frame.Number <> lastFrameNumber
            then { frame with State = Strike }
            else { frame with State = LastStrikeInProgress1 }
            
    let private rollForInProgress firstScore secondScore frame =
        let score = firstScore + secondScore
        if score >= 0 && score < numberOfPins then
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

    let roll score frame =
        if score >= 0 && score <= numberOfPins then
            let rollFun =
                match frame.State with
                | NotStarted ->
                    rollForNotStarted |> ok
                | InProgress firstScore ->
                    rollForInProgress firstScore |> ok
                | Open _ when frame.Number < lastFrameNumber ->
                    rollForFinished |> ok
                | Strike | Spare _ ->
                    rollForFinished |> ok
                | LastStrikeInProgress1 ->
                    rollForLastStrikeInProgress1 |> ok
                | LastStrikeInProgress2 score ->
                    rollForLastStrikeInProgress2 score |> ok
                | _ ->
                    RollAfterLastFrame |> fail

            rollFun |> Trial.map (fun rollFun -> rollFun score frame)
        else
            InvalidScore score |> fail

    let getScores frames =
        frames |> Seq.collect (fun frame -> seq {
            match frame.State with
            | NotStarted ->
                yield! Seq.empty
            | InProgress score ->
                yield score
            | Open (firstScore, secondScore) ->
                yield firstScore
                yield secondScore
            | Strike | LastStrikeInProgress1 ->
                yield numberOfPins
            | LastStrikeInProgress2 score ->
                yield numberOfPins
                yield score
            | LastStrike (firstScore, secondScore) ->
                yield numberOfPins
                yield firstScore
                yield secondScore
            | Spare score ->
                yield score
                yield (numberOfPins - score)
            | LastSpareInProgress score ->
                yield score
                yield (numberOfPins - score)
            | LastSpare (firstScore, secondScore) ->
                yield firstScore
                yield (numberOfPins - firstScore)
                yield secondScore
        })

    let getTotal frameScores =
        frameScores |> List.tryLast |> Option.map (fun score -> score.Total) |> Option.defaultValue 0 
    
    let private add item list =
        list |> List.append [ item ]

    let getTotalScores frames =
        let rec getTotalScores' frames (frameScores : FrameScore list) =
            match frames with
            | [] -> frameScores
            | frame :: otherFrames ->
                let total = frameScores |> getTotal
                let score =
                    match frame.State with
                    | NotStarted ->
                        frameScore
                    | InProgress score ->
                        { frameScore with Total = total + score; FirstRoll = score }
                    | Open (firstScore, secondScore) ->
                        { frameScore with
                            Total = total + firstScore + secondScore
                            FirstRoll = firstScore; SecondRoll = secondScore }
                    | Strike ->
                        { frameScore with
                            Total = total + numberOfPins + (otherFrames |> getScores |> Seq.take 2 |> Seq.fold (+) 0)
                            FirstRoll = numberOfPins }
                    | Spare score ->
                        { frameScore with
                            Total = total + numberOfPins + (otherFrames |> getScores |> Seq.take 1 |> Seq.fold (+) 0)
                            FirstRoll = score
                            SecondRoll = numberOfPins - score }
                    | LastStrikeInProgress1 ->
                        { frameScore with
                            Total = total + numberOfPins
                            FirstRoll = numberOfPins }
                    | LastStrikeInProgress2 score ->
                        { frameScore with
                            Total = total + numberOfPins + score
                            FirstRoll = numberOfPins
                            SecondRoll = score }
                    | LastStrike (firstScore, secondScore) ->
                        { frameScore with
                            Total = total + numberOfPins + firstScore + secondScore
                            FirstRoll = numberOfPins
                            SecondRoll = firstScore
                            ThirdRoll = secondScore }
                    | LastSpareInProgress score ->
                        { frameScore with
                            Total = total + numberOfPins
                            FirstRoll = score
                            SecondRoll = numberOfPins - score }
                    | LastSpare (firstScore, secondScore) ->
                        { frameScore with
                            Total = total + numberOfPins + secondScore
                            FirstRoll = firstScore
                            SecondRoll = numberOfPins - firstScore
                            ThirdRoll = secondScore }

                getTotalScores' otherFrames (frameScores |> add score)

        getTotalScores' frames []
