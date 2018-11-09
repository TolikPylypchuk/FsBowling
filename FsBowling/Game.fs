namespace FsBowling

open FSharpPlus
open FSharpPlus.Data

type Game = {
    Players : NonEmptyList<Player>
}

module Game =

    let players { Players = players } = players
    
    let create playerNames =
        let players =
            playerNames
            |> PlayerName.getNames
            |>> Player.create
            |> sequence

        players
        |>> sequence
        |>> map (fun players -> { Players = players })

    let currentPlayer game =
        let players = game.Players |> NonEmptyList.toList

        let currentFrameNumber =
            players
            |> List.head
            |> Player.lastFrame
            |> Frame.number

        players
        |> List.tryFind (Player.lastFrame >> Frame.number >> (<>) currentFrameNumber)
        |> Option.orElse (players |> List.tryFind (Player.lastFrame >> Frame.isFinished >> not))
        |> Option.defaultValue (players |> List.head)

    let roll score game =
        let players =
            game.Players
            |> map (fun player ->
                if (player |> Player.name) = (game |> currentPlayer |> Player.name)
                then player |> Player.roll score
                else player |> Ok |> result)
            |> sequence

        players
        |>> sequence
        |>> map (fun players -> { game with Players = players })

    let isFinished game =
        game.Players
        |> NonEmptyList.toList
        |> List.last
        |> Player.isFinished
