namespace FsBowling

type Config = {
    NumberOfPins : int
    NumberOfFrames : int
    MaxNameLength : int option
    MaxPlayerCount : int option
}

module Config =

    let getNumberOfPins { NumberOfPins = num } = num
    let NumberOfFrames { NumberOfFrames = num } = num
    let getMaxNameLength { MaxNameLength = length } = length
    let getMaxPlayerCount { MaxPlayerCount = count } = count

    let defaultConfig = {
        NumberOfPins = 10
        NumberOfFrames = 10
        MaxNameLength = None
        MaxPlayerCount = Some 6
    }
