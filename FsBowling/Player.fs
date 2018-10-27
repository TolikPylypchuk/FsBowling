namespace FsBowling

open Chessie.ErrorHandling

type Player = {
    Name : PlayerName
    Frames : Frame list
    CurrentFrame : Frame
}

module Player =
    
    let create name = trial {
        let! frame = Frame.create 1
        return {
            Name = name
            Frames = []
            CurrentFrame = frame
        }
    }

    let validatePlayers players =
        if players |> List.isEmpty then
            PlayerListEmpty |> fail
        else
            let duplicatePlayers =
                players
                |> List.groupBy id
                |> List.filter (fun (_, list) -> list |> List.length <> 1)
                |> List.map fst

            if duplicatePlayers |> List.isEmpty
            then players |> ok
            else duplicatePlayers |> DuplicatePlayers |> fail
