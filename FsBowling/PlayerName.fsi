namespace FsBowling

open FSharpPlus.Data

/// <summary>
/// Represents the name of a player.
/// </summary>
type PlayerName

/// <summary>
/// Contains functions to create and manipulate players' names.
/// </summary>
module PlayerName =

    /// <summary>
    /// Returns the name of a player.
    /// </summary>
    val get : PlayerName -> string

    /// <summary>
    /// Creates a player name from a string.
    /// </summary>
    val create : string -> Reader<Config, Result<PlayerName, BowlingError>>

    /// <summary>
    /// Validates a list of players.
    /// </summary>
    val validatePlayerNames : PlayerName list -> Reader<Config, Result<PlayerName list, BowlingError>>
