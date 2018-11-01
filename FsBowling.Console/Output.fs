module FsBowling.Output

open State
open Frame

let append (suffix : string) str = (str + suffix)

let formatRoll score =
    if score = 0 then "- " else score.ToString().PadRight(2)

let formatScore score =
    state {
        let firstPart =
            if score.FirstRoll = numberOfPins then
                "X "
            elif score.FirstRoll = -1 then
                "  "
            else
                score.FirstRoll |> formatRoll

        do! update <| append "|"
        do! update <| append firstPart

        let secondPart =
            if score.SecondRoll = numberOfPins then
                "X "
            elif score.FirstRoll = numberOfPins then
                "  "
            elif score.FirstRoll + score.SecondRoll = numberOfPins then
                "/ "
            elif score.SecondRoll = -1 then
                "  "
            else
                score.SecondRoll |> formatRoll

        do! update <| append "|"
        do! update <| append secondPart
        do! update <| append "|"

        if score.ThirdRoll > -1 then
            let thirdPart =
                if score.ThirdRoll = numberOfPins
                then "X "
                else score.ThirdRoll |> formatRoll

            do! update <| append thirdPart
            do! update <| append "|"

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
            |> List.map (fun score -> score.Total.ToString().PadRight(5))
            |> reduceWithPipe

        let intermediateLine = String.replicate (firstLine.Length + 2) "-"
        
        do! update <| append intermediateLine
        do! update <| append "\n"

        do! update <| append "|"
        do! update <| append firstLine
        do! update <| append "|\n"

        do! update <| append intermediateLine
        do! update <| append "\n"

        let thirdLine =
            totalScores
            |> List.map formatScore
            |> reduceWithPipe

        do! update <| append thirdLine
        do! update <| append "\n"

        do! update <| append intermediateLine
        do! update <| append "\n"
    } |> run "" |> snd
    
let formatGame game =
    game.Players
    |> List.map formatPlayer
    |> List.reduce (fun players player -> players + "\n" + player)
