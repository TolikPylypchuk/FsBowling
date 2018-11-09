namespace FsBowling

open FSharpPlus
open FSharpPlus.Data

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

[<RequireQualifiedAccess>]
module Frame =

    let state { State = state } = state
    let number { Number = num } = num

    let totalScore { Total = total } = total
    let firstRollScore { FirstRoll = roll } = roll
    let secondRollScore { SecondRoll = roll } = roll
    let thirdRollScore { ThirdRoll = roll } = roll
    
    let frameScore = {
        Total = None
        FirstRoll = None
        SecondRoll = None
        ThirdRoll = None
    }

    let create num =
        Reader.ask |>> (fun config ->
            if num > 0 && num <= (config |> Config.numberOfFrames)
            then { State = NotStarted; Number = num } |> Ok
            else num |> InvalidFrameNumber |> Error)

    let isFinished frame =
        match frame.State with
        | Open _ | Strike | Spare _ | LastStrike _ | LastSpare _ -> true
        | _ -> false

    let isLast frame =
        Reader.ask |>> Config.numberOfFrames |>> (fun numFrames -> frame.Number = numFrames)

    let rollForNotStarted score frame =
        Reader.ask |>> (fun config ->
            if score < (config |> Config.numberOfPins) then
                { frame with State = InProgress score }
            else
                if frame.Number <> (config |> Config.numberOfFrames)
                then { frame with State = Strike }
                else { frame with State = LastStrikeInProgress1 })
    
    let rollForInProgress firstScore secondScore frame =
        Reader.ask |>> (fun config ->
            if (firstScore + secondScore) < (config |> Config.numberOfPins) then
                { frame with State = Open (firstScore, secondScore) }
            else
                if frame.Number <> (config |> Config.numberOfFrames)
                then { frame with State = Spare firstScore }
                else { frame with State = LastSpareInProgress firstScore })
    
    let rollForFinished score frame =
        { State = InProgress score; Number = frame.Number + 1 } |> result

    let rollForLastStrikeInProgress1 score frame =
        { frame with State = LastStrikeInProgress2 score } |> result
        
    let rollForLastStrikeInProgress2 firstScore secondScore frame =
        { frame with State = LastStrike (firstScore, secondScore) } |> result
        
    let rollForLastSpareInProgress firstScore secondScore frame =
        { frame with State = LastSpare (firstScore, secondScore) } |> result

    let roll score frame = monad {
        let! config = Reader.ask
        if score >= 0 && score <= (config |> Config.numberOfPins) then
            let rollFun =
                match frame.State with
                | NotStarted ->
                    rollForNotStarted |> Ok
                | InProgress firstScore ->
                    let totalScore = score + firstScore
                    if totalScore <= (config |> Config.numberOfPins)
                    then rollForInProgress firstScore |> Ok
                    else InvalidScore totalScore |> Error
                | Open _ when frame.Number < (config |> Config.numberOfFrames) ->
                    rollForFinished |> Ok
                | Strike | Spare _ ->
                    rollForFinished |> Ok
                | LastStrikeInProgress1 ->
                    rollForLastStrikeInProgress1 |> Ok
                | LastStrikeInProgress2 score ->
                    rollForLastStrikeInProgress2 score |> Ok
                | LastSpareInProgress firstScore ->
                    rollForLastSpareInProgress firstScore |> Ok
                | _ ->
                    RollAfterLastFrame |> Error

            return rollFun |>> (fun rollFun -> Reader.run (rollFun score frame) config)
        else
            return InvalidScore score |> Error
    }

    let getScores frames = monad {
        let! numPins = Reader.ask |>> Config.numberOfPins
        return
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
                        [ Some numPins; None ]
                    | LastStrikeInProgress1 ->
                        [ Some numPins; None; None ]
                    | LastStrikeInProgress2 score ->
                        [ Some numPins; Some score; None ]
                    | LastStrike (firstScore, secondScore) ->
                        [ Some numPins; Some firstScore; Some secondScore ]
                    | Spare score ->
                        [ Some score; Some (numPins - score) ]
                    | LastSpareInProgress score ->
                        [ Some score; Some (numPins - score) ]
                    | LastSpare (firstScore, secondScore) ->
                        [ Some firstScore; Some (numPins - firstScore); Some secondScore ]
        })
    }
    
    let getTotal =
        List.tryHead >> Option.map (totalScore >> Option.defaultValue 0) >> Option.defaultValue 0

    let getTotalScores frames =
        let rec getTotalScores' (frameScores : FrameScore list) frames = monad {
            let! numPins = Reader.ask |>> Config.numberOfPins
            match frames with
            | [] -> return frameScores |> List.rev
            | frame :: otherFrames ->
                let! scores = otherFrames |> getScores
                let scores = scores |> Seq.choose id
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
                            Total = total + numPins + (scores |> Seq.truncate 2 |> Seq.fold (+) 0) |> Some
                            FirstRoll = numPins |> Some }
                    | Spare score ->
                        { frameScore with
                            Total = total + numPins + (scores |> Seq.tryHead |> Option.defaultValue 0) |> Some
                            FirstRoll = score |> Some
                            SecondRoll = numPins - score |> Some }
                    | LastStrikeInProgress1 ->
                        { frameScore with
                            Total = total + numPins |> Some
                            FirstRoll = numPins |> Some }
                    | LastStrikeInProgress2 score ->
                        { frameScore with
                            Total = total + numPins + score |> Some
                            FirstRoll = numPins |> Some
                            SecondRoll = score |> Some }
                    | LastStrike (firstScore, secondScore) ->
                        { frameScore with
                            Total = total + numPins + firstScore + secondScore |> Some
                            FirstRoll = numPins |> Some
                            SecondRoll = firstScore |> Some
                            ThirdRoll = secondScore |> Some }
                    | LastSpareInProgress score ->
                        { frameScore with
                            Total = total + numPins |> Some
                            FirstRoll = score |> Some
                            SecondRoll = numPins - score |> Some }
                    | LastSpare (firstScore, secondScore) ->
                        { frameScore with
                            Total = total + numPins + secondScore |> Some
                            FirstRoll = firstScore |> Some
                            SecondRoll = numPins - firstScore |> Some
                            ThirdRoll = secondScore |> Some }

                return! otherFrames |> getTotalScores' (score :: frameScores)
        }
        
        frames |> getTotalScores' []
