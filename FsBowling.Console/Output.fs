module FsBowling.Output

open System

open State
open Frame

let append (suffix : string) str = str + suffix

let formatRoll score =
    if score = 0 then "- " else score.ToString().PadRight(2)

let formatScore score =
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

        match score.ThirdRoll with
        | Some roll ->
            let thirdPart = if roll = numberOfPins then "X " else roll |> formatRoll
            do! update <| append "|"
            do! update <| append thirdPart
        | None -> ()

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
            |> List.map (fun score ->
                score.Total
                |> Option.map string
                |> Option.defaultValue String.Empty
                |> fun score -> score.PadRight(5))
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
            |> List.map formatScore
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
