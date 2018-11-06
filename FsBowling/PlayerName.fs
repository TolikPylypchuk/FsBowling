namespace FsBowling

open System
open FSharpPlus
open FSharpPlus.Data
open Chessie.ErrorHandling

type PlayerName = PlayerName of string

module PlayerName =
    
    let get (PlayerName name) = name

    let create (name : string) = monad {
        let name = name.Trim()
        let! config = Reader.ask

        return
            if name = String.Empty then
                PlayerNameEmpty |> fail
            else
                match config.MaxNameLength with
                | Some maxLength when name.Length > maxLength -> name |> PlayerNameTooLong |> fail
                | _ -> PlayerName name |> ok
    }

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

    let createPlayerNames names = monad {
        let! config = Reader.ask
        return
            names
            |> List.map create
            |> List.map (flip Reader.run config)
            |> Trial.sequence
            |> Trial.bind validatePlayerNames
    }
