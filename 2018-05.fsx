let diff = int('a') - int('A')
let stack =  new System.Collections.Generic.Stack<FSharp.Core.Option<int>>()
stack.Push None

let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.Read ()
}

let reactPolymer value = 
    match stack.Peek() with
    | Some v when (abs(v - value)).Equals(diff) -> stack.Pop() |> ignore
    | _ -> stack.Push(Some value)

readFile "2018-05" |> Seq.iter reactPolymer

stack.Count - 1
