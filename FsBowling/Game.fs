namespace FsBowling

open Chessie.ErrorHandling

type Game = {
    Players : Player list
}

module Game =

    let create playerNames =
        playerNames
        |> PlayerName.validatePlayerNames
        >>= (fun playerNames -> playerNames |> List.map Player.create |> Trial.sequence)
        |> Trial.map (fun players -> { Players = players })
