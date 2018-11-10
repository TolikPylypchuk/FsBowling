namespace FsBowling

open FSharpPlus.Data

/// Represents the name of a player.
type PlayerName

/// Represents a valid list of player names.
type ValidatedPlayerNames

/// Contains functions to create and manipulate players' names.
module PlayerName =

    /// Returns the name of a player.
    val get : PlayerName -> string
    
    /// Returns a list of valid names.
    val getNames : ValidatedPlayerNames -> NonEmptyList<PlayerName>

    /// Creates a player name from a string.
    val create : string -> Reader<Config, Result<PlayerName, BowlingError>>

    /// Validates a list of players.
    val validatePlayerNames : PlayerName list -> Reader<Config, Result<ValidatedPlayerNames, BowlingError>>
