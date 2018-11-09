namespace FsBowling

open FSharpPlus.Data

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
    | DuplicatePlayers of NonEmptyList<string>
    | InvalidScore of int
    | RollAfterLastFrame
