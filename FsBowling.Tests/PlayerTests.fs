﻿module FsBowling.PlayerTests

open Xunit

[<Fact>]
let ``Creating a player should always be successful`` () =
    let name = PlayerName "test"

    let expected = {
        Name = name
        Frames = [ {
            State = NotStarted
            Number = 1
        } ]
    }

    Player.create name |> shouldBeSuccess expected

[<Fact>]
let ``A list of players should be valid when it's non-empty and when there are no duplications`` () =
    let players = [ "one"; "two"; "three" ] |> List.map PlayerName
    players |> PlayerName.validatePlayerNames |> shouldBeSuccess players
    
[<Fact>]
let ``A list of players should not be valid when it's empty`` () =
    [] |> PlayerName.validatePlayerNames |> shouldBeFailure [ PlayerListEmpty ]
        
[<Fact>]
let ``A list of players should not be valid when there are duplications`` () =
    let players = [ "one"; "one"; "two"; "three"; "three" ] |> List.map PlayerName
    players |> PlayerName.validatePlayerNames |> shouldBeFailure [ [ "one"; "three" ] |> DuplicatePlayers ]
