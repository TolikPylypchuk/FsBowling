[<RequireQualifiedAccess>]
module FsBowling.Output

open System
open FSharpPlus
open FSharpPlus.Data

let add = Writer.tell

let addLine = flip (+) "\n" >> Writer.tell

let pad num (str : string) =
    str.PadRight(num)

let formatRoll score =
    if score = 0 then "- " else score |> string |> pad 2

let formatScore isLastFrame score = monad {
    let! numPins = Reader.ask |>> Config.numberOfPins
    
    return monad {
        let format roll =
            match roll with
            | Some roll when roll = numPins -> "X "
            | Some roll -> roll |> formatRoll
            | None -> "  "

        do! add <| format (score |> Frame.firstRollScore)

        let secondRoll =
            match score |> Frame.firstRollScore, score |> Frame.secondRollScore with
            | Some roll1, Some roll2 when roll1 + roll2 = numPins -> "/ "
            | Some roll1, Some roll2 when roll1 = numPins && roll2 = numPins -> "X "
            | _, Some roll -> roll |> formatRoll
            | _, None -> "  "

        do! add <| "|" + secondRoll

        if isLastFrame then
            do! add "|"
            do! add <| format (score |> Frame.thirdRollScore)
    } |> Writer.exec
}

let formatPlayer player = monad {
    let! config = Reader.ask
    let! totalScores = player |> Player.frames |> NonEmptyList.toList |> Frame.getTotalScores

    return monad {
        do! addLine (player |> Player.name |> PlayerName.get)

        let numFrames = config |> Config.numberOfFrames

        let firstLine =
            totalScores
            |> List.mapi (fun index ->
                Frame.totalScore
                >> Option.map string
                >> Option.defaultValue String.Empty
                >> pad (if index + 1 = numFrames then 8 else 5))
            |> String.concat "|"

        let intermediateLine = String.replicate (firstLine |> String.length |> (+) 2) "-"
        
        do! addLine intermediateLine
        do! addLine <| "|" + firstLine + "|"
        do! addLine intermediateLine
        
        let secondLine =
            totalScores
            |> List.mapi (fun index -> formatScore (index + 1 = numFrames))
            |> sequence
            |> flip Reader.run config
            |> String.concat "|"

        do! addLine <| "|" + secondLine + "|"
        do! addLine intermediateLine
    } |> Writer.exec
}
    
let formatGame game =
    game
    |> Game.players
    |> NonEmptyList.map formatPlayer
    |> sequence
    |>> String.concat "\n"

let formatError error = monad {
    let! config = Reader.ask
    return
        match error with
        | InvalidFrameNumber num ->
            sprintf "%i is an invalid frame number." num
        | PlayerNameEmpty ->
            "The player's name is empty."
        | PlayerNameTooLong _ ->
            sprintf "The name is too long. A player's name should not exceed %i characters." (config |> Config.maxNameLength |> Option.defaultValue 0)
        | PlayerListEmpty ->
            "The player list is empty."
        | TooManyPlayers players ->
            sprintf "%i is too many players. The number of players must be at most %i." players (config |> Config.maxNameLength |> Option.defaultValue 0)
        | DuplicatePlayers players ->
            match players.Tail with
            | [] ->
                sprintf "The name %s is duplicated." players.Head
            | _ ->
                players
                |> NonEmptyList.toList
                |> String.concat ", "
                |> sprintf "The names %s are duplicated."
        | InvalidScore score ->
            sprintf "%i is an invalid score. A score of a frame must be less than or equal to %i." score (config |> Config.numberOfPins)
        | RollAfterLastFrame ->
            "The player rolled past the last frame."
}
