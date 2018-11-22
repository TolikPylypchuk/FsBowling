module FsBowling.PlayerNameTests

open System
open FSharpPlus
open FSharpPlus.Data
open Expecto
open FsCheck

[<Tests>]
let tests =
    testList "Player name tests" [
        testProperty "A player name must be non-empty and not longer than the max length" <| fun configParams (NonNull name) ->
            (createConfig configParams) >>==> fun config ->
                let maxLength = config |> Config.maxNameLength |> Option.defaultValue Int32.MaxValue

                match PlayerName.create name |> with' config with
                | Ok playerName ->
                    let name = name.Trim()
                    (name |> String.length) > 0 &&
                    (name |> String.length) <= maxLength &&
                    (playerName |> PlayerName.get) = name
                | Error error ->
                    let name = name.Trim()
                    (error = PlayerNameEmpty) = (name = "") &&
                    (error = PlayerNameTooLong name) = (name |> String.length > maxLength)

        testProperty "A list of players must not be empty or too long and must not contain duplicates" <| fun configParams (names : NonNull<string> list) ->
            let names = names |> List.map (fun name -> name.Get)

            (createConfig configParams) >>==> fun config ->
                names |> List.map (createPlayerName >> with' config) |> sequence >==> fun names ->
                    let maxPlayerCount = config |> Config.maxPlayerCount |> Option.defaultValue Int32.MaxValue
                    let duplicatePlayers =
                        names
                        |> List.groupBy id
                        |> List.filter (snd >> List.length >> (<>) 1)
                        |> List.map fst

                    match PlayerName.validatePlayerNames names |> with' config with
                    | Ok playerNames ->
                        let playerNames = playerNames |> PlayerName.getNames |> NonEmptyList.toList
                        (playerNames |> List.length) <= maxPlayerCount &&
                        (duplicatePlayers |> List.length) = 0 &&
                        playerNames = names
                    | Error error ->
                        (error = PlayerListEmpty) = (names |> List.length = 0) &&
                        (error = TooManyPlayers (names |> List.length)) = (names |> List.length > maxPlayerCount) &&
                        match duplicatePlayers |> List.map PlayerName.get with
                        | head :: tail -> error = DuplicatePlayers (NonEmptyList.create head tail)
                        | [] -> match error with DuplicatePlayers _ -> false | _ -> true
    ]
