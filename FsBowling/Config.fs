namespace FsBowling

open FSharpPlus
open FSharpPlus.Data

type Config = {
    NumberOfPins : int
    NumberOfFrames : int
    MaxNameLength : int option
    MaxPlayerCount : int option
}

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

    let private positiveOrElse error num =
        if num > 0 then num |> Success else [ error num ] |> Failure

    let private forOption validationFunc = Option.map Success >> Option.map (Validation.bind validationFunc) >> sequence

    let private validateNumPins = positiveOrElse InvalidNumberOfPins
    let private validateNumFrames = positiveOrElse InvalidNumberOfFrames
    let private validateMaxNameLength = positiveOrElse InvalidMaxNameLength |> forOption
    let private validateMaxPlayerCount = positiveOrElse InvalidMaxPlayerCount |> forOption

    let private doCreate numPins numFrames maxNameLength maxPlayerCount = {
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
