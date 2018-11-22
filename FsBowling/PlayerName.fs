namespace FsBowling

open System
open FSharpPlus
open FSharpPlus.Data

type PlayerName = PlayerName of string

type ValidatedPlayerNames = ValidatedPlayerNames of NonEmptyList<PlayerName>

[<RequireQualifiedAccess>]
module PlayerName =
    
    let get (PlayerName name) = name

    let getNames (ValidatedPlayerNames names) = names

    let create (name : string) = monad {
        let name = name.Trim()
        let! config = Reader.ask

        return
            if name = String.Empty then
                PlayerNameEmpty |> Error
            else
                match config |> Config.maxNameLength with
                | Some maxLength when name.Length > maxLength -> name |> PlayerNameTooLong |> Error
                | _ -> PlayerName name |> Ok
    }

    let validatePlayerNames players =
        Reader.ask |>> (fun config ->
            if players |> List.isEmpty then
                PlayerListEmpty |> Error
            else
                match config |> Config.maxPlayerCount with
                | Some count when players |> List.length > count ->
                    players |> List.length |> TooManyPlayers |> Error
                | _ ->
                    let duplicatePlayers =
                        players
                        |> List.groupBy id
                        |> List.filter (snd >> List.length >> (<>) 1)
                        |> List.map fst

                    let makeNonEmpty (lst : 'a list) = NonEmptyList.create lst.Head lst.Tail 

                    if duplicatePlayers |> List.isEmpty
                    then players |> makeNonEmpty |> ValidatedPlayerNames |> Ok
                    else duplicatePlayers |> List.map get |> makeNonEmpty |> DuplicatePlayers |> Error)
