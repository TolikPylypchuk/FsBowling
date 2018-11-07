namespace FsBowling

open FSharpPlus
open FSharpPlus.Data
open Chessie.ErrorHandling

type Player = {
    Name : PlayerName
    Frames : Frame list
}

module Player =
    
    let create name =
        Frame.create 1
        |>> (Trial.map (fun frame -> {
                Name = name
                Frames = [ frame ]
            }))

    let getName player =
        player.Name |> PlayerName.get

    let roll score player = monad {
        let! config = Reader.ask
        return trial {
            let reversedFrames = player.Frames |> List.rev
            let! frame = reversedFrames |> List.head |> Frame.roll score |> Reader.run <| config
            let frames = frame :: (reversedFrames |> List.tail)
            let result =
                if frame |> Frame.isFinished && frame.Number <> config.NumberOfFrames
                then { State = NotStarted; Number = frame.Number + 1 } :: frames
                else frames
                |> List.rev
        
            return { player with Frames = result }
        }
    }

    let lastFrame player =
        player.Frames |> List.last

    let isFinished player = monad {
        let! config = Reader.ask
        let frame = player |> lastFrame
        return frame.Number = config.NumberOfFrames && frame |> Frame.isFinished
    }
