module FsBowling.Input

open System
open Chessie.ErrorHandling

let inputNumPlayers () =
    printf "Enter the number of players: "

    let rec inputNumPlayers' () =
        let success, num = Console.ReadLine() |> Int32.TryParse
        if success && num > 0
        then
            num
        else
            printf "\nThe number is invalid. Please try again: "
            inputNumPlayers' ()

    inputNumPlayers' ()

let inputPlayer index =
    printf "\nEnter the name of player #%i: " <| index + 1

    let rec inputPlayer' () =
        match Console.ReadLine() |> PlayerName.create with
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
    let (PlayerName currentPlayerName) = (game |> Game.currentPlayer).Name
    printf "%s rolls with score: " currentPlayerName

    let rec inputRoll' () =
        let success, score = Console.ReadLine() |> Int32.TryParse
        if success && score >= 0 then
            printfn ""
            score
        else
            printf "The score cannot be a negative number. Please try again: "
            inputRoll' ()

    inputRoll' ()
