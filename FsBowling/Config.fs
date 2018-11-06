namespace FsBowling

type Config = {
    MaxNameLength : int option
    NumberOfPins : int
    NumberOfFrames : int
}

module Config =

    let defaultConfig = {
        MaxNameLength = None
        NumberOfPins = 10
        NumberOfFrames = 10
    }
