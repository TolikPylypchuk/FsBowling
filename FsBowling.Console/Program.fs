module FsBowling.Program

open FSharpPlus
open FSharpPlus.Data
open Chessie.ErrorHandling

open Input
open Output

let rec createGame () = monad {
    let! players = inputPlayers ()
    match! players |> Game.create with
    | Ok (game, _) -> return game
    | Bad errors ->
        do! errors |> printErrors
        return! createGame ()
}

let rec play game = monad {
    let! isGameFinished = game |> Game.isFinished
    if isGameFinished then
        return 0
    else
        match! game |> Game.roll (game |> inputRoll) with
        | Ok (game, _) ->
            let! formattedGame = game |> formatGame
            printfn "%s" formattedGame
            return! play game
        | Bad errors ->
            do! errors |> printErrors
            printfn ""
            return! play game
}

[<EntryPoint>]
let main _ =
    printfn "Welcome to FsBowling Console!\n"
    |> createGame
    |> Reader.bind play
    |> flip Reader.run Config.defaultConfig
