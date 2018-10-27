namespace FsBowling

type BowlingError =
    | InvalidFrameNumber of int
    | PlayerNameEmpty
    | PlayerNameTooLong of string
    | PlayerListEmpty
    | DuplicatePlayers of PlayerName list
    | InvalidScore of int
