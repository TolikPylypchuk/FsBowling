module FsBowling.PlayerTests

open Chessie.ErrorHandling

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Creating a player should always be successful`` () =
    let name = PlayerName "test"

    let expected = {
        Name = name
        Frames = []
        LastFrame = None
        CurrentFrame = NormalFrame.NotStarted |> Frame.Normal
        CurrentFrameNumber = 1
    }

    Player.create name |> shouldBeSuccess expected

[<Fact>]
let ``A list of players should be valid when it's non-empty and when there are no duplications`` () =
    let players = [ "one"; "two"; "three" ] |> List.map PlayerName
    players |> Player.validatePlayers |> shouldBeSuccess players
    
[<Fact>]
let ``A list of players should not be valid when it's empty`` () =
    [] |> Player.validatePlayers |> shouldBeFailure [ PlayerListEmpty ]
        
[<Fact>]
let ``A list of players should not be valid when there are duplications`` () =
    let players = [ "one"; "one"; "two"; "three"; "three" ] |> List.map PlayerName
    players |> Player.validatePlayers |> shouldBeFailure [ [ "one"; "three" ] |> List.map PlayerName |> DuplicatePlayers ]
