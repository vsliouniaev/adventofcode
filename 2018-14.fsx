open System.Collections.Generic
open System.Linq

let checkEnd (list:List<int>) (target:int[]) =
  list.Skip(list.Count - target.Length).Take(target.Length).ToArray() = target

let size i =
     i |> float |> log10 |> floor |> (+) 1.0 |> int

let cook (data:List<int>) idx1 idx2 =    
     let n = data.[idx1] + data.[idx2]
     if n <> 0 then
          let d = size n
          for x in d-1..-1..0 do 
               data.Add( n / (pown 10 x) % 10)
               if checkEnd data [|3;2;0;8;5;1|]
               then failwithf "%i" (data.Count - 6)
          if data.Count % 10000 = 0 then printfn "%i" data.Count
          (data, d)
     else 
          data.Add(0)
          (data, 1)
     
let move (data:List<int>) idx =
    let newIdx = data.[idx] + 1 + idx
    newIdx % data.Count

let rec ticks data i1 i2 max current =
    let (data, n) = cook data i1 i2
    let i1 = move data i1
    let i2 = move data i2
    if max <= current
    then (data, i1, i2)
    else ticks data i1 i2 max (current + 1)

let rec go data i1 i2 = 
     let (data, i1, i2) = ticks data i1 i2 1 0     
     if false then ()
     else go data i1 i2

let num = 320851
let l:List<int> = new List<int>([|3;7|])
// let (s,_,_) = ticks l 0 1 (num + 10) 0
// s.Skip(num).Take(10).ToArray() // 7116398711

//15586739
go l 0 1 //20316365

// This simply does not work in debug mode. Suspect this is because tail-call optimization is not available
