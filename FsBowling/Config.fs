namespace FsBowling

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
