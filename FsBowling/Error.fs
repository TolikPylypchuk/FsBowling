namespace FsBowling

type ConfigError =
    | InvalidNumberOfPins of int
    | InvalidNumberOfFrames of int
    | InvalidMaxNameLength of int
    | InvalidMaxPlayerCount of int

type BowlingError =
    | InvalidFrameNumber of int
    | PlayerNameEmpty
    | PlayerNameTooLong of string
    | PlayerListEmpty
    | TooManyPlayers
    | DuplicatePlayers of string list
    | InvalidScore of int
    | RollAfterLastFrame
