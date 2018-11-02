module FsBowling.Program

open Chessie.ErrorHandling

open Game

[<EntryPoint>]
let main _ =
    let result =
        [ "one"; "two"; "three" ]
        |> PlayerName.createPlayerNames
        >>= create
        >>= roll 2
        >>= roll 6
        >>= roll 10
        >>= roll 7
        >>= roll 1
        >>= roll 3
        >>= roll 5
        >>= roll 5
        |> Trial.map Output.formatGame

    match result with
    | Ok (result, _) -> printfn "%s" result
    | Bad errors -> printfn "%A" errors

    0
