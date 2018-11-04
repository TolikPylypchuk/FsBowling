module FsBowling.Output

open System

open State
open Frame

let append (suffix : string) str = str + suffix

let formatRoll score =
    if score = 0 then "- " else score.ToString().PadRight(2)

let formatScore isLastFrame score =
    state {
        let firstPart =
            match score.FirstRoll with
            | Some roll when roll = numberOfPins -> "X "
            | Some roll -> roll |> formatRoll
            | None -> "  "

        do! update <| append firstPart

        let secondPart =
            match score.FirstRoll, score.SecondRoll with
            | Some roll1, Some roll2 when roll1 + roll2 = numberOfPins -> "/ "
            | Some roll1, Some roll2 when roll1 = numberOfPins && roll2 = numberOfPins -> "X "
            | _, Some roll -> roll |> formatRoll
            | _, None -> "  "

        do! update <| append "|"
        do! update <| append secondPart

        if isLastFrame then
            match score.ThirdRoll with
            | Some roll ->
                let thirdPart = if roll = numberOfPins then "X " else roll |> formatRoll
                do! update <| append "|"
                do! update <| append thirdPart
            | None ->
                do! update <| append "|  "

    } |> run "" |> snd

let formatPlayer player =
    state {
        let totalScores = player.Frames |> Frame.getTotalScores

        let (PlayerName name) = player.Name
        do! update <| append name
        do! update <| append "\n"

        let reduceWithPipe = List.reduce (fun acc score -> acc + "|" + score)

        let firstLine =
            totalScores
            |> List.mapi (fun index score ->
                score.Total
                |> Option.map string
                |> Option.defaultValue String.Empty
                |> fun score -> score.PadRight(if index + 1 = Frame.lastFrameNumber then 8 else 5))
            |> reduceWithPipe

        let intermediateLine = String.replicate (firstLine.Length + 2) "-"
        
        do! update <| append intermediateLine
        do! update <| append "\n"

        do! update <| append "|"
        do! update <| append firstLine
        do! update <| append "|\n"

        do! update <| append intermediateLine
        do! update <| append "\n"

        let secondLine =
            totalScores
            |> List.mapi (fun index -> formatScore (index + 1 = Frame.lastFrameNumber))
            |> reduceWithPipe

        do! update <| append "|"
        do! update <| append secondLine
        do! update <| append "|\n"

        do! update <| append intermediateLine
        do! update <| append "\n"
    } |> run "" |> snd
    
let formatGame game =
    game.Players
    |> List.map formatPlayer
    |> List.reduce (fun players player -> players + "\n" + player)

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
    List.map formatError
    >> List.iter (printfn "%s")
