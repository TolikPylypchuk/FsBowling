[<RequireQualifiedAccess>]
module FsBowling.Input

open System
open FSharpPlus
open FSharpPlus.Data

let private read = Console.ReadLine

let private readInt () =
    let success, result = Console.ReadLine () |> Int32.TryParse
    if success then Some result else None

let readNumPlayers () =
    printf "Enter the number of players: "

    let rec readNumPlayers' () = monad {
        let! config = Reader.ask
        match readInt (), config.MaxPlayerCount with
            | Some num, None when num > 0 ->
                return num
            | Some num, Some count when num > 0 && num <= count ->
                return num
            | _, Some count ->
                printf "\nThe number of players must be positive and at most %i. Please try again: " count
                return! readNumPlayers' ()
            | _, None ->
                printf "\nThe number of players must be positive. Please try again: "
                return! readNumPlayers' ()
    }

    readNumPlayers' ()

let readPlayer index =
    printf "\nEnter the name of player #%i: " <| index + 1

    let rec readPlayer' () = monad {
        match! read () |> PlayerName.create with
        | Ok player -> return player
        | Error error ->
            printfn "\nThe player name is invalid."
            do! error |> Output.formatError |>> printfn "%s"
            printf "\nPlease try again: "
            return! readPlayer' ()
    }

    readPlayer' ()

let rec readPlayers () = monad {
    let! numPlayers = readNumPlayers ()
    let! names =
        readPlayer
        |> List.init numPlayers
        |> sequence

    match! names |> PlayerName.validatePlayerNames with
    | Ok players ->
        printfn ""
        return players
    | Error error ->
        printfn "\nThe player list is invalid."
        do! error |> Output.formatError |>> printfn "%s"
        printfn "Please try again.\n"
        return! readPlayers ()
}

let readRoll game =
    printf "%s rolls with score: " (game |> Game.currentPlayer |> Player.name |> PlayerName.get)

    let rec readRoll' () =
        match readInt () with
        | Some score when score >= 0 ->
            printfn ""
            score
        | _ ->
            printf "The score must be a non-negative number. Please try again: "
            readRoll' ()

    readRoll' ()
