namespace FsBowling

open FSharpPlus.Data

/// <summary>
/// Represents the configuration of a bowling game.
/// </summary>
type Config

module Config =
    
    /// <summary>
    /// Returns the number of pins.
    /// </summary>
    val numberOfPins : Config -> int
    
    /// <summary>
    /// Returns the number of frames.
    /// </summary>
    val numberOfFrames : Config -> int
    
    /// <summary>
    /// Returns the max name length, if any.
    /// </summary>
    val maxNameLength : Config -> int option
    
    /// <summary>
    /// Returns the max player count, if any.
    /// </summary>
    val maxPlayerCount : Config -> int option
    
    /// <summary>
    /// The default game configuration.
    /// </summary>
    val defaultConfig : Config

    /// <summary>
    /// Creates a configuration.
    /// </summary>
    val create : int -> int -> int option -> int option -> Validation<ConfigError list, Config>
