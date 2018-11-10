namespace FsBowling

open FSharpPlus.Data

/// Represents an error which may occur during the creation of the configuration.
type ConfigError =

    /// The number of pins is not positive.
    | InvalidNumberOfPins of int

    /// The number of frames is not positive.
    | InvalidNumberOfFrames of int
    
    /// The max name length is not positive.
    | InvalidMaxNameLength of int
    
    /// The max player count is not positive.
    | InvalidMaxPlayerCount of int

/// Represents an error which may occur during the calculation of a bowling score.
type BowlingError =

    /// The frame number is invalid.
    | InvalidFrameNumber of int

    /// The player name is empty.
    | PlayerNameEmpty
    
    /// The player name exceeds the maximum player name length.
    | PlayerNameTooLong of string
    
    /// The list of players is empty.
    | PlayerListEmpty
    
    /// Too many players.
    | TooManyPlayers
    
    /// The list of players contains duplicate names.
    | DuplicatePlayers of NonEmptyList<string>
    
    /// The score is invalid.
    | InvalidScore of int
    
    /// The roll occured when the player finished all their frames.
    | RollAfterLastFrame
