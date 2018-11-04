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

let rec playGame game =
    if game |> Game.isFinished then
        ()
    else
        let score = inputRoll ()
        match game |> Game.roll score with
        | Ok (game, _) ->
            game |> formatGame |> printfn "%s"
            game |> playGame
        | Bad errors ->
            errors |> Output.printErrors
            printfn ""
            game |> playGame

[<EntryPoint>]
let main _ =
    printfn "Welcome to FsBowling Console!\n"

    createGame () |> playGame

    0
