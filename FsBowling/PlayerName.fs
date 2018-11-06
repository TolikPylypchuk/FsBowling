namespace FsBowling

open System
open Chessie.ErrorHandling

type PlayerName = PlayerName of string

module PlayerName =

    let maxNameLength = 30
    
    let get (PlayerName name) = name

    let create (name : string) =
        let name = name.Trim()
        if name = String.Empty then
            PlayerNameEmpty |> fail
        elif name.Length > maxNameLength then
            name |> PlayerNameTooLong |> fail
        else
            PlayerName name |> ok

    let validatePlayerNames players =
        if players |> List.isEmpty then
            PlayerListEmpty |> fail
        else
            let duplicatePlayers =
                players
                |> List.groupBy id
                |> List.filter (fun (_, list) -> list |> List.length <> 1)
                |> List.map fst

            if duplicatePlayers |> List.isEmpty
            then players |> ok
            else duplicatePlayers |> List.map get |> DuplicatePlayers |> fail

    let createPlayerNames names =
        names
        |> List.map create
        |> Trial.sequence
        |> Trial.bind validatePlayerNames
