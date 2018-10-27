module FsBowling.PlayerName

open System
open Chessie.ErrorHandling

let maxNameLength = 30

let create (name : string) =
    let name = name.Trim()
    if name = String.Empty then
        PlayerNameEmpty |> fail
    elif name.Length > maxNameLength then
        name |> PlayerNameTooLong |> fail
    else
        PlayerName name |> ok
