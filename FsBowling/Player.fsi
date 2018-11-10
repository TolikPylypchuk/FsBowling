namespace FsBowling

open FSharpPlus.Data

/// Represents a player.
type Player

/// Contains functions to create and manipulate players.
module Player =
    
    /// Returns a player's name.
    val name : Player -> PlayerName
    
    /// Returns a list of player's frames.
    val frames : Player -> NonEmptyList<Frame>
    
    /// Creates a player with a specified name.
    val create : PlayerName -> Reader<Config, Result<Player, BowlingError>>
    
    /// Calculates a new list of player's frames based on the score of a roll.
    val roll : int -> Player -> Reader<Config, Result<Player, BowlingError>>
    
    /// Returns the last frame of a player.
    val lastFrame : Player -> Frame
    
    /// Determines whether a player has finished playing.
    val isFinished : Player -> Reader<Config, bool>
