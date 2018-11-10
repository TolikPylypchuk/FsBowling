namespace FsBowling

open FSharpPlus.Data

/// Represents the state of a frame.
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
    
/// Represents a frame.
type Frame

/// Represents a frame score.
type FrameScore

/// Contains functions to create and manipulate frames, their states and scores.
module Frame =

    /// Returns a frame's state.
    val state : Frame -> FrameState
    
    /// Returns a frame's number.
    val number : Frame -> int
    
    /// Returns a frame score's total score.
    val totalScore : FrameScore -> int option
    
    /// Returns a frame score's first roll score.
    val firstRollScore : FrameScore -> int option
    
    /// Returns a frame score's second roll score.
    val secondRollScore : FrameScore -> int option
    
    /// Returns a frame score's third roll score.
    val thirdRollScore : FrameScore -> int option
    
    /// Creates a not started frame with a specified number.
    val create : int -> Reader<Config, Result<Frame, BowlingError>>
    
    /// Determines whether a frame is finished.
    val isFinished : Frame -> bool
    
    /// Determines whether a frame is the last frame.
    val isLast : Frame -> Reader<Config, bool>
    
    /// Calculates a new frame state based on the score of a roll.
    val roll : int -> Frame -> Reader<Config, Result<Frame, BowlingError>>
    
    /// Returns a sequence of scores of rolls.
    val getScores : seq<Frame> -> Reader<Config, seq<int option>>
    
    /// Returns a list of scores of frames.
    val getTotalScores : Frame list -> Reader<Config, FrameScore list>
