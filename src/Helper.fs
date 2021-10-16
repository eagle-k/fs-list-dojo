module Helper

open System

// Adapted from http://www.fssnip.net/7V5/title/Emulating-Idrisstyle-holes
type Hole = Hole
[<CompilerMessage("not yet implemented", 130)>]
let (?) (_ : Hole) (id : string) : 'T = 
    sprintf "Incomplete hole '%s : %O'" id typeof<'T>
    |> NotImplementedException
    |> raise

