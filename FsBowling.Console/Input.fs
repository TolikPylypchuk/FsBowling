module FsBowling.Input

open System
open FSharpPlus
open FSharpPlus.Data

open Output

let read = Console.ReadLine

let readInt () =
    let success, result = Console.ReadLine () |> Int32.TryParse
    if success then Some result else None

let inputNumPlayers () =
    printf "Enter the number of players: "

    let rec inputNumPlayers' () =
        match readInt () with
        | Some num when num > 0 ->
            num
        | _ ->
            printf "\nThe number of players must be positive. Please try again: "
            inputNumPlayers' ()

    inputNumPlayers' ()

let inputPlayer index =
    printf "\nEnter the name of player #%i: " <| index + 1

    let rec inputPlayer' () = monad {
        match! read () |> PlayerName.create with
        | Ok player -> return player
        | Error error ->
            printfn "\nThe player name is invalid."
            do! error |> formatError |>> printfn "%s"
            printf "\nPlease try again: "
            return! inputPlayer' ()
    }

    inputPlayer' ()

let rec inputPlayers () = monad {
    let! config = Reader.ask
    let numPlayers = inputNumPlayers ()
    let names =
        List.init numPlayers inputPlayer
        |> List.map (flip Reader.run config)
        |> PlayerName.validatePlayerNames

    match names with
    | Ok players ->
        printfn ""
        return players
    | Error error ->
        printfn "The player list is invalid."
        do! error |> formatError |>> printfn "%s"
        printfn "Please try again.\n"

        return! inputPlayers ()
}

let inputRoll game =
    printf "%s rolls with score: " (game |> Game.currentPlayer |> Player.getName)

    let rec inputRoll' () =
        match readInt () with
        | Some score when score >= 0 ->
            printfn ""
            score
        | _ ->
            printf "The score must be a non-negative number. Please try again: "
            inputRoll' ()

    inputRoll' ()
