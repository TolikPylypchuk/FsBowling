module FsBowling.Input

open System
open Chessie.ErrorHandling

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

    let rec inputPlayer' () =
        match read () |> PlayerName.create with
        | Ok (player, _) -> player
        | Bad errors ->
            printfn "\nThe player name is invalid."
            errors |> Output.printErrors
            printf "\nPlease try again: "
            inputPlayer' ()

    inputPlayer' ()

let rec inputPlayers () =
    let numPlayers = inputNumPlayers ()

    match List.init numPlayers inputPlayer |> PlayerName.validatePlayerNames with
    | Ok (players, _) ->
        printfn ""
        players
    | Bad errors ->
        printfn "The player list is invalid."
        errors |> Output.printErrors
        printfn "Please try again.\n"

        inputPlayers ()

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
