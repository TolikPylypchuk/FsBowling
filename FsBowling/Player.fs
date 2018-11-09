namespace FsBowling

open FSharpPlus
open FSharpPlus.Data

type Player = {
    Name : PlayerName
    Frames : NonEmptyList<Frame>
}

[<RequireQualifiedAccess>]
module Player =
    
    let name { Name = name } = name
    let frames { Frames = frames } = frames

    let create name =
        Frame.create 1
        |>> (map (fun frame -> {
                Name = name
                Frames = frame |> NonEmptyList.singleton
            }))

    let roll score player =
        Reader.ask |>> (fun config -> monad {
            let reversedFrames = player.Frames |> NonEmptyList.toList |> List.rev

            let! frame =
                reversedFrames
                |> List.head
                |> Frame.roll score
                |> Reader.run <| config

            let frames = frame :: (reversedFrames |> List.tail)
            let number = frame |> Frame.number

            let! result =
                if frame |> Frame.isFinished && number <> (config |> Config.numberOfFrames)
                then
                    let result = number + 1 |> Frame.create |> Reader.run <| config
                    result |>> fun frame -> frame :: frames
                else frames |> Ok
                |>> List.rev
            
            return { player with Frames = NonEmptyList.create result.Head result.Tail }
        })

    let lastFrame player =
        player.Frames |> NonEmptyList.toList |> List.last

    let isFinished player =
        Reader.ask |>> (fun config ->
            let frame = player |> lastFrame
            (frame |> Frame.number) = (config |> Config.numberOfFrames) && frame |> Frame.isFinished)
