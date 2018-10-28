namespace FsBowling

type BowlingError =
    | InvalidFrameNumber of int
    | PlayerNameEmpty
    | PlayerNameTooLong of string
    | PlayerListEmpty
    | DuplicatePlayers of string list
    | InvalidScore of int
    | RollAfterLastFrame
