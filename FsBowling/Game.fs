namespace FsBowling

open Chessie.ErrorHandling

type Game = {
    Players : Player list
}

module Game =

    let create players =
        players
        |> Player.validatePlayers
        >>= (fun players -> players |> List.map Player.create |> Trial.sequence)
        |> Trial.lift (fun players -> { Players = players })
