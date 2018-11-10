namespace FsBowling

open FSharpPlus.Data

/// Represents the configuration of a bowling game.
type Config

/// Contains functions and values to create and manipulate configurations of games.
module Config =
    
    /// Returns the number of pins.
    val numberOfPins : Config -> int
    
    /// Returns the number of frames.
    val numberOfFrames : Config -> int
    
    /// Returns the max name length, if any.
    val maxNameLength : Config -> int option
    
    /// Returns the max player count, if any.
    val maxPlayerCount : Config -> int option
    
    /// The default game configuration.
    val defaultConfig : Config

    /// Creates a configuration.
    val create : int -> int -> int option -> int option -> Validation<ConfigError list, Config>
