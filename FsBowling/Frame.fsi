namespace FsBowling

open FSharpPlus.Data

/// <summary>
/// Represents the state of a frame.
/// </summary>
type FrameState =
    | NotStarted
    | InProgress of int
    | Open of int * int
    | Strike
    | LastStrikeInProgress1
    | LastStrikeInProgress2 of int
    | LastStrike of int * int
    | Spare of int
    | LastSpareInProgress of int
    | LastSpare of int * int
    
/// <summary>
/// Represents a frame.
/// </summary>
type Frame

/// <summary>
/// Represents a frame score.
/// </summary>
type FrameScore

/// <summary>
/// Contains functions to create and manipulate frames, their states and scores.
/// </summary>
module Frame =

    /// <summary>
    /// Returns a frame's state.
    /// </summary>
    val state : Frame -> FrameState
    
    /// <summary>
    /// Returns a frame's number.
    /// </summary>
    val number : Frame -> int
    
    /// <summary>
    /// Returns a frame score's total score.
    /// </summary>
    val totalScore : FrameScore -> int option
    
    /// <summary>
    /// Returns a frame score's first roll score.
    /// </summary>
    val firstRollScore : FrameScore -> int option
    
    /// <summary>
    /// Returns a frame score's second roll score.
    /// </summary>
    val secondRollScore : FrameScore -> int option
    
    /// <summary>
    /// Returns a frame score's third roll score.
    /// </summary>
    val thirdRollScore : FrameScore -> int option
    
    /// <summary>
    /// Creates a not started frame with a specified number.
    /// </summary>
    val create : int -> Reader<Config, Result<Frame, BowlingError>>
    
    /// <summary>
    /// Determines whether a frame is finished.
    /// </summary>
    val isFinished : Frame -> bool
    
    /// <summary>
    /// Determines whether a frame is the last frame.
    /// </summary>
    val isLast : Frame -> Reader<Config, bool>
    
    /// <summary>
    /// Calculates a new frame state based on the score of a roll.
    /// </summary>
    val roll : int -> Frame -> Reader<Config, Result<Frame, BowlingError>>
    
    /// <summary>
    /// Returns a sequence of scores of rolls.
    /// </summary>
    val getScores : seq<Frame> -> Reader<Config, seq<int option>>
    
    /// <summary>
    /// Returns a list of scores of frames.
    /// </summary>
    val getTotalScores : Frame list -> Reader<Config, FrameScore list>
