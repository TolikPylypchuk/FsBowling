namespace FsBowling

open Chessie.ErrorHandling

type Game = {
    Players : Player list
}

module Game =

    let create playerNames =
        playerNames
        |> List.map Player.create
        |> Trial.sequence
        |> Trial.map (fun players -> { Players = players })

    let currentPlayer game =
        let currentFrameNumber =
            game.Players
            |> List.head
            |> Player.lastFrame
            |> fun frame -> frame.Number

        game.Players
        |> List.tryFind (Player.lastFrame >> (fun frame -> frame.Number) >> (<>) currentFrameNumber)
        |> Option.orElse (game.Players |> List.tryFind (Player.lastFrame >> Frame.isFinished >> not))
        |> Option.defaultValue (game.Players |> List.head)

    let roll score game =
        game.Players
        |> List.map (fun player ->
            if player.Name = (game |> currentPlayer).Name
            then player |> Player.roll score
            else player |> ok)
        |> Trial.sequence
        |> Trial.map (fun players -> { game with Players = players })

    let isFinished game =
        game.Players
        |> List.last
        |> Player.isFinished
