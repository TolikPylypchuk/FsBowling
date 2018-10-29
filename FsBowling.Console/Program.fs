module FsBowling.Program

open Chessie.ErrorHandling

[<EntryPoint>]
let main _ =
    let result =
        [ "one"; "two"; "three" ]
        |> PlayerName.createPlayerNames
        >>= Game.create
        |> Trial.map Output.formatGame

    match result with
    | Ok (result, _) -> printfn "%s" result
    | Bad errors -> printfn "%A" errors

    0
