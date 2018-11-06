module FsBowling.Output

open System
open FSharpPlus
open FSharpPlus.Data

open Frame

let add = Writer.tell

let addLine = flip (+) Environment.NewLine >> Writer.tell

let pad num (str : string) =
    str.PadRight(num)

let formatRoll score =
    if score = 0 then "- " else score |> string |> pad 2

let formatScore isLastFrame score =
    monad {
        let format roll =
            match roll with
            | Some roll when roll = numberOfPins -> "X "
            | Some roll -> roll |> formatRoll
            | None -> "  "

        do! add <| format score.FirstRoll

        let secondRoll =
            match score.FirstRoll, score.SecondRoll with
            | Some roll1, Some roll2 when roll1 + roll2 = numberOfPins -> "/ "
            | Some roll1, Some roll2 when roll1 = numberOfPins && roll2 = numberOfPins -> "X "
            | _, Some roll -> roll |> formatRoll
            | _, None -> "  "

        do! add <| "|" + secondRoll

        if isLastFrame then
            do! add "|"
            do! add <| format score.ThirdRoll
    } |> Writer.exec

let formatPlayer player =
    monad {
        let totalScores = player.Frames |> Frame.getTotalScores

        do! addLine (player |> Player.getName) 

        let firstLine =
            totalScores
            |> List.mapi (fun index score ->
                score.Total
                |> Option.map string
                |> Option.defaultValue String.Empty
                |> pad (if index + 1 = Frame.lastFrameNumber then 8 else 5))
            |> String.concat "|"

        let intermediateLine = String.replicate (firstLine.Length + 2) "-"
        
        do! addLine intermediateLine
        do! addLine <| "|" + firstLine + "|"
        do! addLine intermediateLine

        let secondLine =
            totalScores
            |> List.mapi (fun index -> formatScore (index + 1 = Frame.lastFrameNumber))
            |> String.concat "|"

        do! addLine <| "|" + secondLine + "|"
        do! addLine intermediateLine
    } |> Writer.exec
    
let formatGame game =
    game.Players
    |> List.map formatPlayer
    |> String.concat "\n"

let formatError =
    function
    | InvalidFrameNumber num ->
        sprintf "%i is an invalid frame number." num
    | PlayerNameEmpty ->
        "The player's name is empty."
    | PlayerNameTooLong _ ->
        sprintf "The name is too long. A player's name should not exceed %i characters." PlayerName.maxNameLength
    | PlayerListEmpty ->
        "The player list is empty."
    | DuplicatePlayers players ->
        match players with
        | [ player ] ->
            sprintf "The name %s is duplicated." player
        | [] ->
            "An error occured - no names are duplicated but the app thinks they are."
        | _ ->
            players
            |> List.reduce (fun acc item -> acc + ", " + item)
            |> sprintf "The names %s are duplicated."
    | InvalidScore score ->
        sprintf "%i is an invalid score. A score of a frame must be less than or equal to %i." score Frame.numberOfPins
    | RollAfterLastFrame ->
        "The player rolled past the last frame."

let printErrors =
    List.map formatError >> List.iter (printfn "%s")
