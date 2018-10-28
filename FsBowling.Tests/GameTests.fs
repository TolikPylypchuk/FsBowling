module FsBowling.GameTests

open Xunit
open FsUnit.Xunit
open Chessie.ErrorHandling

let createPlayers players =
    players |> List.map PlayerName.create |> Trial.sequence >>= PlayerName.validatePlayerNames

[<Fact>]
let ``Creating a game should be successful when the list of players is non-empty and when there are no duplications`` () =
    let playerNames = createPlayers [ "one"; "two"; "three" ]
    
    let expected = trial {
        let! names = playerNames
        return {
            Players = names |> List.map (fun name ->
                {
                    Name = name
                    Frames = []
                    CurrentFrame = {
                        State = NotStarted
                        Number = 1
                    }
                })
        }
    }
    
    playerNames >>= Game.create |> should equal expected
    
[<Fact>]
let ``Creating a game should not be successful when the list of players is empty`` () =
    createPlayers [] >>= Game.create |> shouldBeFailure [ PlayerListEmpty ]
        
[<Fact>]
let ``Creating a game should not be successful when there are duplicate players`` () =
    createPlayers [ "one"; "one"; "two"; "three"; "three" ]
    >>= Game.create
    |> shouldBeFailure [ [ "one"; "three" ] |> DuplicatePlayers ]
