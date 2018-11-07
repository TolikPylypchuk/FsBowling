module FsBowling.Program

open FSharpPlus
open FSharpPlus.Data

open Input
open Output

let rec createGame () = monad {
    let! players = inputPlayers ()
    match! players |> Game.create with
    | Ok game -> return game
    | Error error ->
        do! error |> formatError |>> printfn "%s"
        return! createGame ()
}

let rec play game = monad {
    let! isGameFinished = game |> Game.isFinished
    if isGameFinished then
        return 0
    else
        match! game |> Game.roll (game |> inputRoll) with
        | Ok game ->
            let! formattedGame = game |> formatGame
            printfn "%s" formattedGame
            return! play game
        | Error error ->
            do! error |> formatError |>> printfn "%s"
            printfn ""
            return! play game
}

[<EntryPoint>]
let main _ =
    printfn "Welcome to FsBowling Console!\n"
    |> createGame
    >>= play
    |> flip Reader.run Config.defaultConfig
