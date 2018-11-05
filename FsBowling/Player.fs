namespace FsBowling

open Chessie.ErrorHandling

type Player = {
    Name : PlayerName
    Frames : Frame list
}

module Player =
    
    let create name = trial {
        let! frame = Frame.create 1
        return {
            Name = name
            Frames = [ frame ]
        }
    }

    let roll score player = trial {
        let reversedFrames = player.Frames |> List.rev
        let! frame = reversedFrames |> List.head |> Frame.roll score
        let frames = frame :: (reversedFrames |> List.tail)
        let result =
            if frame |> Frame.isFinished && frame.Number <> Frame.lastFrameNumber
            then { State = NotStarted; Number = frame.Number + 1 } :: frames
            else frames
            |> List.rev
        
        return { player with Frames = result }
    }

    let lastFrame player =
        player.Frames |> List.last

    let isFinished =
        lastFrame >> fun frame -> frame.Number = Frame.lastFrameNumber && frame |> Frame.isFinished
