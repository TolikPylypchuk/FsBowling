namespace FsBowling

open FSharpPlus
open FSharpPlus.Data

type Player = {
    Name : PlayerName
    Frames : Frame list
}

module Player =
    
    let create name =
        Frame.create 1
        |>> (map (fun frame -> {
                Name = name
                Frames = [ frame ]
            }))

    let getName player =
        player.Name |> PlayerName.get

    let roll score player =
        Reader.ask |>> (fun config -> monad {
            let reversedFrames = player.Frames |> List.rev

            let! frame =
                reversedFrames
                |> List.head
                |> Frame.roll score
                |> Reader.run <| config

            let frames = frame :: (reversedFrames |> List.tail)

            let result =
                if frame |> Frame.isFinished && frame.Number <> config.NumberOfFrames
                then { State = NotStarted; Number = frame.Number + 1 } :: frames
                else frames
                |> List.rev
        
            return { player with Frames = result }
        })

    let lastFrame player =
        player.Frames |> List.last

    let isFinished player =
        Reader.ask |>> (fun config ->
            let frame = player |> lastFrame
            frame.Number = config.NumberOfFrames && frame |> Frame.isFinished)
