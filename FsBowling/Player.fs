namespace FsBowling

open Chessie.ErrorHandling

type Player = {
    Name : PlayerName
    Frames : Frame list
}

module Player =
    
    let create name = trial {
        let! frame = Frame.create 1
        return {
            Name = name
            Frames = [ frame ]
        }
    }
