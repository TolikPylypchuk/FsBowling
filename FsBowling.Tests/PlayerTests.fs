module FsBowling.PlayerTests

open Chessie.ErrorHandling

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Creating a player should always be successful`` () =
    let name = PlayerName "test"
    match Player.create name with
    | Ok (player, []) -> player |> should equal {
            Name = name
            Frames = []
            LastFrame = None
            CurrentFrame = NormalFrame.NotStarted |> Frame.Normal
            CurrentFrameNumber = 1
        }
    | _ -> sprintf "Failed creating a player with name %A" name |> failTest

[<Fact>]
let ``A list of players should be valid when it's non-empty and when there are no duplications`` () =
    let players = [ "one"; "two"; "three" ] |> List.map PlayerName
    match players |> Player.validatePlayers with
    | Ok (result, []) -> result |> should equal players
    | _ -> sprintf "Failed validating players %A" players |> failTest
    
[<Fact>]
let ``A list of players should not be valid when it's empty`` () =
    match [] |> Player.validatePlayers with
    | Ok _ -> "Succeeded validating an empty player list" |> failTest
    | Bad errors ->
        match errors with
        | error :: [] -> error |> should equal PlayerListEmpty
        | _ -> sprintf "Unexpected error %A" errors |> failTest
        
[<Fact>]
let ``A list of players should not be valid when there are duplications`` () =
    let players = [ "one"; "one"; "two"; "three"; "three" ] |> List.map PlayerName
    match players |> Player.validatePlayers with
    | Ok _ -> "Succeeded validating a player list with duplications" |> failTest
    | Bad errors ->
        match errors with
        | DuplicatePlayers duplications :: [] ->
            duplications |> should equal ([ "one"; "three" ] |> List.map PlayerName)
        | _ -> sprintf "Unexpected error %A" errors |> failTest
