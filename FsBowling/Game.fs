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

    let roll score game =
        let currentFrameNumber = game.Players |> List.head |> Player.getLastFrameNumber

        let currentPlayerIndex =
            game.Players
            |> List.tryFindIndex (Player.getLastFrameNumber >> (<>) currentFrameNumber)
            |> Option.defaultValue 0
        
        game.Players
        |> List.mapi (fun index player ->
            if index = currentPlayerIndex
            then player |> Player.roll score
            else player |> ok)
        |> Trial.sequence
        |> Trial.map (fun players -> { game with Players = players })

    let isFinished game =
        game.Players
        |> List.last
        |> Player.isFinished
