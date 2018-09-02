namespace FsBowling

type BowlingError =
    | InvalidFrameNumber of int
    | PlayerListEmpty
    | DuplicatePlayers of PlayerName list
    | InvalidScore of int
