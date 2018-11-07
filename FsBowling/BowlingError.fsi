namespace FsBowling

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
    | DuplicatePlayers of string list
    
    /// <summary>
    /// The score is invalid.
    /// </summary>
    | InvalidScore of int
    
    /// <summary>
    /// The roll occured when the player finished all their frames.
    /// </summary>
    | RollAfterLastFrame
