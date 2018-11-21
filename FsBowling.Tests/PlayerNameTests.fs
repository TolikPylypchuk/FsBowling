module FsBowling.PlayerNameTests

open System
open Expecto
open FsCheck

[<Tests>]
let tests =
    testList "Player name tests" [
        testProperty "A player name must be non-empty and not longer than the max length" <| fun configParams name ->
            (name |> isNull |> not) ==> lazy ((createConfig configParams) >>==> fun config ->
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
                    (error = PlayerNameTooLong name) = (name |> String.length > maxLength))
    ]
