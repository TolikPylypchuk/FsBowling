namespace FsBowling

open FSharpPlus
open FSharpPlus.Data
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
    
    let private frameScore = {
        Total = None
        FirstRoll = None
        SecondRoll = None
        ThirdRoll = None
    }

    let create num = monad {
        let! config = Reader.ask
        return
            if num > 0 && num <= config.NumberOfFrames
            then { State = NotStarted; Number = num } |> ok
            else num |> InvalidFrameNumber |> fail
    }

    let isFinished frame =
        match frame.State with
        | Open _ | Strike | Spare _ | LastStrike _ | LastSpare _ -> true
        | _ -> false

    let isLast frame =
        Reader.ask |>> (fun config -> frame.Number = config.NumberOfFrames)

    let private rollForNotStarted score frame = monad {
        let! config = Reader.ask
        return
            if score < config.NumberOfPins then
                { frame with State = InProgress score }
            else
                if frame.Number <> config.NumberOfFrames
                then { frame with State = Strike }
                else { frame with State = LastStrikeInProgress1 }
    }
    
    let private rollForInProgress firstScore secondScore frame = monad {
        let! config = Reader.ask
        return
            if (firstScore + secondScore) < config.NumberOfPins then
                { frame with State = Open (firstScore, secondScore) }
            else
                if frame.Number <> config.NumberOfFrames
                then { frame with State = Spare firstScore }
                else { frame with State = LastSpareInProgress firstScore }
    }
    
    let private rollForFinished score frame =
        { State = InProgress score; Number = frame.Number + 1 } |> result

    let private rollForLastStrikeInProgress1 score frame =
        { frame with State = LastStrikeInProgress2 score } |> result
        
    let private rollForLastStrikeInProgress2 firstScore secondScore frame =
        { frame with State = LastStrike (firstScore, secondScore) } |> result
        
    let private rollForLastSpareInProgress firstScore secondScore frame =
        { frame with State = LastSpare (firstScore, secondScore) } |> result

    let roll score frame = monad {
        let! config = Reader.ask
        if score >= 0 && score <= config.NumberOfPins then
            let rollFun =
                match frame.State with
                | NotStarted ->
                    rollForNotStarted |> ok
                | InProgress firstScore ->
                    let totalScore = score + firstScore
                    if totalScore <= config.NumberOfPins
                    then rollForInProgress firstScore |> ok
                    else InvalidScore totalScore |> fail
                | Open _ when frame.Number < config.NumberOfFrames ->
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

            return rollFun |> Trial.map (fun rollFun -> Reader.run (rollFun score frame) config)
        else
            return InvalidScore score |> fail
    }

    let getScores frames = monad {
        let! config = Reader.ask
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
                        [ Some config.NumberOfPins; None ]
                    | LastStrikeInProgress1 ->
                        [ Some config.NumberOfPins; None; None ]
                    | LastStrikeInProgress2 score ->
                        [ Some config.NumberOfPins; Some score; None ]
                    | LastStrike (firstScore, secondScore) ->
                        [ Some config.NumberOfPins; Some firstScore; Some secondScore ]
                    | Spare score ->
                        [ Some score; Some (config.NumberOfPins - score) ]
                    | LastSpareInProgress score ->
                        [ Some score; Some (config.NumberOfPins - score) ]
                    | LastSpare (firstScore, secondScore) ->
                        [ Some firstScore; Some (config.NumberOfPins - firstScore); Some secondScore ]
        })
    }
    
    let getTotal =
        List.tryHead >> Option.map (fun score -> score.Total |> Option.defaultValue 0) >> Option.defaultValue 0

    let getTotalScores frames =
        let rec getTotalScores' (frameScores : FrameScore list) frames = monad {
            let! config = Reader.ask
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
                            Total = total + config.NumberOfPins + (scores |> Seq.truncate 2 |> Seq.fold (+) 0) |> Some
                            FirstRoll = config.NumberOfPins |> Some }
                    | Spare score ->
                        { frameScore with
                            Total = total + config.NumberOfPins + (scores |> Seq.tryHead |> Option.defaultValue 0) |> Some
                            FirstRoll = score |> Some
                            SecondRoll = config.NumberOfPins - score |> Some }
                    | LastStrikeInProgress1 ->
                        { frameScore with
                            Total = total + config.NumberOfPins |> Some
                            FirstRoll = config.NumberOfPins |> Some }
                    | LastStrikeInProgress2 score ->
                        { frameScore with
                            Total = total + config.NumberOfPins + score |> Some
                            FirstRoll = config.NumberOfPins |> Some
                            SecondRoll = score |> Some }
                    | LastStrike (firstScore, secondScore) ->
                        { frameScore with
                            Total = total + config.NumberOfPins + firstScore + secondScore |> Some
                            FirstRoll = config.NumberOfPins |> Some
                            SecondRoll = firstScore |> Some
                            ThirdRoll = secondScore |> Some }
                    | LastSpareInProgress score ->
                        { frameScore with
                            Total = total + config.NumberOfPins |> Some
                            FirstRoll = score |> Some
                            SecondRoll = config.NumberOfPins - score |> Some }
                    | LastSpare (firstScore, secondScore) ->
                        { frameScore with
                            Total = total + config.NumberOfPins + secondScore |> Some
                            FirstRoll = firstScore |> Some
                            SecondRoll = config.NumberOfPins - firstScore |> Some
                            ThirdRoll = secondScore |> Some }

                return! otherFrames |> getTotalScores' (score :: frameScores)
        }
        
        frames |> getTotalScores' []
