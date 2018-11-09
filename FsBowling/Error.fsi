namespace FsBowling

open FSharpPlus.Data

/// <summary>
/// Represents an error which may occur during the creation of the configuration.
/// </summary>
type ConfigError =

    /// <summary>
    /// The number of pins is not positive.
    /// </summary>
    | InvalidNumberOfPins of int

    /// <summary>
    /// The number of frames is not positive.
    /// </summary>
    | InvalidNumberOfFrames of int
    
    /// <summary>
    /// The max name length is not positive.
    /// </summary>
    | InvalidMaxNameLength of int
    
    /// <summary>
    /// The max player count is not positive.
    /// </summary>
    | InvalidMaxPlayerCount of int

/// <summary>
/// Represents an error which may occur during the calculation of a bowling score.
/// </summary>
type BowlingError =

    /// <summary>
    /// The frame number is invalid.
    /// </summary>
    | InvalidFrameNumber of int

    /// <summary>
    /// The player name is empty.
    /// </summary>
    | PlayerNameEmpty
    
    /// <summary>
    /// The player name exceeds the maximum player name length.
    /// </summary>
    | PlayerNameTooLong of string
    
    /// <summary>
    /// The list of players is empty.
    /// </summary>
    | PlayerListEmpty
    
    /// <summary>
    /// Too many players.
    /// </summary>
    | TooManyPlayers
    
    /// <summary>
    /// The list of players contains duplicate names.
    /// </summary>
    | DuplicatePlayers of NonEmptyList<string>
    
    /// <summary>
    /// The score is invalid.
    /// </summary>
    | InvalidScore of int
    
    /// <summary>
    /// The roll occured when the player finished all their frames.
    /// </summary>
    | RollAfterLastFrame
