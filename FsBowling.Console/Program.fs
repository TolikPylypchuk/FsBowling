module FsBowling.Program

open Chessie.ErrorHandling

open PlayerName
open Game

[<EntryPoint>]
let main _ =
    let result =
        [ "Player 1" ]
        |> createPlayerNames
        >>= create
        >>= roll 2
        >>= roll 6
        >>= roll 10
        >>= roll 7
        >>= roll 1
        >>= roll 3
        >>= roll 5
        >>= roll 4
        >>= roll 5
        >>= roll 6
        >>= roll 0
        >>= roll 3
        >>= roll 5
        >>= roll 7
        >>= roll 2
        >>= roll 8
        >>= roll 1
        >>= roll 9
        >>= roll 1
        >>= roll 1
        |> Trial.map Output.formatGame

    match result with
    | Ok (result, _) -> printfn "%s" result
    | Bad errors -> printfn "%A" errors

    0
