namespace FsBowling

type Config = {
    MaxNameLength : int option
    NumberOfPins : int
    NumberOfFrames : int
}

module Config =

    let getMaxNameLength { MaxNameLength = length } = length
    let getNumberOfPins { NumberOfPins = num } = num
    let NumberOfFrames { NumberOfFrames = num } = num

    let defaultConfig = {
        MaxNameLength = None
        NumberOfPins = 10
        NumberOfFrames = 10
    }
