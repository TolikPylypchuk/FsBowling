namespace FsBowling

open FSharpPlus.Data

/// <summary>
/// Represents a player.
/// </summary>
type Player

/// <summary>
/// Contains functions to create and manipulate players.
/// </summary>
module Player =
    
    /// <summary>
    /// Returns a player's name.
    /// </summary>
    val name : Player -> PlayerName
    
    /// <summary>
    /// Returns a list of player's frames.
    /// </summary>
    val frames : Player -> NonEmptyList<Frame>
    
    /// <summary>
    /// Creates a player with a specified name.
    /// </summary>
    val create : PlayerName -> Reader<Config, Result<Player, BowlingError>>
    
    /// <summary>
    /// Calculates a new list of player's frames based on the score of a roll.
    /// </summary>
    val roll : int -> Player -> Reader<Config, Result<Player, BowlingError>>
    
    /// <summary>
    /// Returns the last frame of a player.
    /// </summary>
    val lastFrame : Player -> Frame
    
    /// <summary>
    /// Determines whether a player has finished playing.
    /// </summary>
    val isFinished : Player -> Reader<Config, bool>
