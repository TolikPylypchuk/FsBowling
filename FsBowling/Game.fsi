namespace FsBowling

open FSharpPlus.Data

/// Represents a game of bowling.
type Game

/// Contains functions to create and manipulate bowling games.
module Game =
    
    /// Returns a game's player list.
    val players : Game -> NonEmptyList<Player>
    
    /// Creates a game with specified players.
    val create : ValidatedPlayerNames -> Reader<Config, Result<Game, BowlingError>>
    
    /// Creates a game with specified players.
    val currentPlayer : Game -> Player
    
    /// Calculates the next game state based on the score of a roll.
    val roll : int -> Game -> Reader<Config, Result<Game, BowlingError>>
    
    /// Determines whether a game is finished.
    val isFinished : Game -> Reader<Config, bool>
