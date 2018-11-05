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
        let currentFrameNumber =
            game.Players
            |> List.head
            |> fun player -> player.Frames
            |> List.last
            |> fun frame -> frame.Number

        let currentPlayerIndex =
            game.Players
            |> List.tryFindIndex (fun player ->
                let frame = player.Frames |> List.last
                frame.Number <> currentFrameNumber)
            |> Option.orElse
                (game.Players
                |> List.tryFindIndex (fun player ->
                    let frame = player.Frames |> List.last
                    frame |> Frame.isFinished |> not))
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
