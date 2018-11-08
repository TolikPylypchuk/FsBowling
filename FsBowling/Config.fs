namespace FsBowling

open FSharpPlus
open FSharpPlus.Data

type Config = {
    NumberOfPins : int
    NumberOfFrames : int
    MaxNameLength : int option
    MaxPlayerCount : int option
}

[<RequireQualifiedAccess>]
module Config =

    let numberOfPins { NumberOfPins = num } = num
    let numberOfFrames { NumberOfFrames = num } = num
    let maxNameLength { MaxNameLength = length } = length
    let maxPlayerCount { MaxPlayerCount = count } = count
    
    let defaultConfig = {
        NumberOfPins = 10
        NumberOfFrames = 10
        MaxNameLength = None
        MaxPlayerCount = Some 6
    }

    let positiveOrElse error num =
        if num > 0 then num |> Success else [ error num ] |> Failure

    let forOption validationFunc = Option.map Success >> Option.map (Validation.bind validationFunc) >> sequence

    let validateNumPins = positiveOrElse InvalidNumberOfPins
    let validateNumFrames = positiveOrElse InvalidNumberOfFrames
    let validateMaxNameLength = positiveOrElse InvalidMaxNameLength |> forOption
    let validateMaxPlayerCount = positiveOrElse InvalidMaxPlayerCount |> forOption

    let doCreate numPins numFrames maxNameLength maxPlayerCount = {
        NumberOfPins = numPins
        NumberOfFrames = numFrames
        MaxNameLength = maxNameLength
        MaxPlayerCount = maxPlayerCount
    }

    let create numPins numFrames maxNameLength maxPlayerCount =
        let numPins = validateNumPins numPins
        let numFrames = validateNumFrames numFrames
        let maxNameLength = validateMaxNameLength maxNameLength
        let maxPlayerCount = validateMaxPlayerCount maxPlayerCount

        doCreate <!> numPins <*> numFrames <*> maxNameLength <*> maxPlayerCount
