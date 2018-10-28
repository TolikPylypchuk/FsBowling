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
        ThirdRoll = 0
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
        frameScores |> List.tryLast |> Trial.failIfNone InvalidFrameScores |> Trial.map (fun score -> score.Total)

    let private getTotalScoresForInProgress score frameScores =
        frameScores
        |> getTotal
        |> Trial.map (fun total ->
            frameScores |> List.append [ { frameScore with Total = total + score; FirstRoll = score } ])

    let private getTotalScoresForOpen firstScore secondScore frameScores =
        frameScores
        |> getTotal
        |> Trial.map (fun total ->
            frameScores
            |> List.append [ { frameScore with
                                Total = total + firstScore + secondScore
                                FirstRoll = firstScore; SecondRoll = secondScore } ])

    let private getTotalScoresForStrike otherFrames frameScores =
        frameScores
        |> getTotal
        |> Trial.map (fun total -> (total, otherFrames |> getScores |> Seq.take 2 |> Seq.reduce (+)))
        |> Trial.map (fun (total, additional) ->
            frameScores
            |> List.append [ { frameScore with
                                Total = total + numberOfPins + additional
                                FirstRoll = numberOfPins } ])

    let private getTotalScoresForSpare otherFrames score frameScores =
        frameScores
        |> getTotal
        |> Trial.map (fun total -> (total, otherFrames |> getScores |> Seq.head))
        |> Trial.map (fun (total, additional) ->
            frameScores
            |> List.append [ { frameScore with
                                Total = total + numberOfPins + additional
                                FirstRoll = score
                                SecondRoll = numberOfPins - score } ])

    let private getTotalScoresForLastStrikeInProgress1 frameScores =
        frameScores
        |> getTotal
        |> Trial.map (fun total ->
            frameScores
            |> List.append [ { frameScore with
                                Total = total + numberOfPins
                                FirstRoll = numberOfPins } ])

    let private getTotalScoresForLastStrikeInProgress2 score frameScores =
        frameScores
        |> getTotal
        |> Trial.map (fun total ->
            frameScores
            |> List.append [ { frameScore with
                                Total = total + numberOfPins + score
                                FirstRoll = numberOfPins
                                SecondRoll = score } ])

    let private getTotalScoresForLastStrike firstScore secondScore frameScores =
        frameScores
        |> getTotal
        |> Trial.map (fun total ->
            frameScores
            |> List.append [ { frameScore with
                                Total = total + numberOfPins + firstScore + secondScore
                                FirstRoll = numberOfPins
                                SecondRoll = firstScore
                                ThirdRoll = secondScore } ])

    let private getTotalScoresForLastSpareInProgress score frameScores =
        frameScores
        |> getTotal
        |> Trial.map (fun total ->
            frameScores
            |> List.append [ { frameScore with
                                Total = total + numberOfPins
                                FirstRoll = score
                                SecondRoll = numberOfPins - score } ])

    let private getTotalScoresForLastSpare firstScore secondScore frameScores =
        frameScores
        |> getTotal
        |> Trial.map (fun total ->
            frameScores
            |> List.append [ { frameScore with
                                Total = total + numberOfPins + secondScore
                                FirstRoll = firstScore
                                SecondRoll = numberOfPins - firstScore
                                ThirdRoll = secondScore } ])

    let getTotalScores frames =
        let rec getTotalScores' frames frameScores =
            match frames with
            | [] -> frameScores |> ok
            | frame :: otherFrames ->
                let getTotalScores =
                    match frame.State with
                    | NotStarted ->
                        ok
                    | InProgress score ->
                        getTotalScoresForInProgress score
                    | Open (firstScore, secondScore) ->
                        getTotalScoresForOpen firstScore secondScore
                    | Strike ->
                        getTotalScoresForStrike otherFrames
                    | Spare score ->
                        getTotalScoresForSpare otherFrames score
                    | LastStrikeInProgress1 ->
                        getTotalScoresForLastStrikeInProgress1
                    | LastStrikeInProgress2 score ->
                        getTotalScoresForLastStrikeInProgress2 score
                    | LastStrike (firstScore, secondScore) ->
                        getTotalScoresForLastStrike firstScore secondScore
                    | LastSpareInProgress score ->
                        getTotalScoresForLastSpareInProgress score
                    | LastSpare (firstScore, secondScore) ->
                        getTotalScoresForLastSpare firstScore secondScore

                getTotalScores frameScores >>= getTotalScores' otherFrames

        getTotalScores' frames []
