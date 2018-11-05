module FsBowling.Program

open Chessie.ErrorHandling

open Input
open Output

let rec createGame () =
    match inputPlayers () |> Game.create with
    | Ok (game, _) -> game
    | Bad errors ->
        errors |> printErrors
        createGame ()

let rec play game =
    if game |> Game.isFinished then
        0
    else
        match game |> Game.roll (game |> inputRoll) with
        | Ok (game, _) ->
            game |> formatGame |> printfn "%s"
            play game
        | Bad errors ->
            errors |> printErrors
            printfn ""
            play game

[<EntryPoint>]
let main _ =
    printfn "Welcome to FsBowling Console!\n"
    |> createGame
    |> play
