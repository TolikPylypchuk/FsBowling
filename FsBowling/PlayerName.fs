namespace FsBowling

open System
open FSharpPlus
open FSharpPlus.Data

type PlayerName = PlayerName of string

module PlayerName =
    
    let get (PlayerName name) = name

    let create (name : string) = monad {
        let name = name.Trim()
        let! config = Reader.ask

        return
            if name = String.Empty then
                PlayerNameEmpty |> Error
            else
                match config.MaxNameLength with
                | Some maxLength when name.Length > maxLength -> name |> PlayerNameTooLong |> Error
                | _ -> PlayerName name |> Ok
    }

    let validatePlayerNames players =
        if players |> List.isEmpty then
            PlayerListEmpty |> Error
        else
            let duplicatePlayers =
                players
                |> List.groupBy id
                |> List.filter (snd >> List.length >> (<>) 1)
                |> List.map fst

            if duplicatePlayers |> List.isEmpty
            then players |> Ok
            else duplicatePlayers |> List.map get |> DuplicatePlayers |> Error
