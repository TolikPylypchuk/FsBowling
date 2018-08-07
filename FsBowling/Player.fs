namespace FsBowling

open Chessie.ErrorHandling

type Player = {
    Name : PlayerName
    Frames : Frame list
    LastFrame : LastFrame option
    CurrentFrame : Frame
    CurrentFrameNumber : int
}

module Player =
    
    let create name = trial {
        let one = 1
        let! frame = Frame.create one
        return {
            Name = name
            Frames = []
            LastFrame = None
            CurrentFrame = frame
            CurrentFrameNumber = one
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
