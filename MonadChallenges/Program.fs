open MCPrelude
open Set2
open Set3

[<EntryPoint>]
let main argv =
    let listMaybe : (int Maybe) List = [None; Some 1]
    let list = [1I; 2I; 3I]
    let pairs = [1, 'a'; 2, 'b'; 3, 'c']
    printfn "%A" (allCombs3' (fun a b c -> a, b, c) [1;2] [3;4] [5;6])
    0
