namespace FsBowling

open FSharpPlus.Data

/// <summary>
/// Represents the name of a player.
/// </summary>
type PlayerName

/// <summary>
/// Represents a valid list of player names.
/// </summary>
type ValidatedPlayerNames

/// <summary>
/// Contains functions to create and manipulate players' names.
/// </summary>
module PlayerName =

    /// <summary>
    /// Returns the name of a player.
    /// </summary>
    val get : PlayerName -> string
    
    /// <summary>
    /// Returns a list of valid names.
    /// </summary>
    val getNames : ValidatedPlayerNames -> NonEmptyList<PlayerName>

    /// <summary>
    /// Creates a player name from a string.
    /// </summary>
    val create : string -> Reader<Config, Result<PlayerName, BowlingError>>

    /// <summary>
    /// Validates a list of players.
    /// </summary>
    val validatePlayerNames : PlayerName list -> Reader<Config, Result<ValidatedPlayerNames, BowlingError>>
