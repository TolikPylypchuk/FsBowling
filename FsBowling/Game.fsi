namespace FsBowling

open FSharpPlus.Data

/// <summary>
/// Represents a game of bowling.
/// </summary>
type Game

/// <summary>
/// Contains functions to create and manipulate bowling games.
/// </summary>
module Game =
    
    /// <summary>
    /// Returns a game's player list.
    /// </summary>
    val players : Game -> Player list
    
    /// <summary>
    /// Creates a game with specified players.
    /// </summary>
    val create : PlayerName list -> Reader<Config, Result<Game, BowlingError>>
    
    /// <summary>
    /// Creates a game with specified players.
    /// </summary>
    val currentPlayer : Game -> Player
    
    /// <summary>
    /// Calculates the next game state based on the score of a roll.
    /// </summary>
    val roll : int -> Game -> Reader<Config, Result<Game, BowlingError>>
    
    /// <summary>
    /// Determines whether the game is finished.
    /// </summary>
    val isFinished : Game -> Reader<Config, bool>
