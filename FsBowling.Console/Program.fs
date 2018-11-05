module FsBowling.Program

open Chessie.ErrorHandling

open Input
open Output

let rec createGame () =
    match inputPlayers () |> Game.create with
    | Ok (game, _) -> game
    | Bad errors ->
        errors |> Output.printErrors
        createGame ()

let rec play game =
    if game |> Game.isFinished then
        ()
    else
        let score = game |> inputRoll
        match game |> Game.roll score with
        | Ok (game, _) ->
            game |> formatGame |> printfn "%s"
            game |> play
        | Bad errors ->
            errors |> Output.printErrors
            printfn ""
            game |> play

[<EntryPoint>]
let main _ =
    printfn "Welcome to FsBowling Console!\n"
    |> createGame
    |> play
    0
