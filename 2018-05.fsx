open System.Collections.Generic
let caseInsensitiveEqual x y = abs(int(x) - int(y)) = int('a') - int('A')

let stackCount (stack:Stack<Option<'a>>) = stack.Count - 1 
let makeStack () = 
        let stack = new Stack<Option<char>>()
        stack.Push None
        stack

let stackToSeq (stack:Stack<Option<'a>>) = seq {
        while stack.Peek() <> None do
                yield stack.Pop().Value
}

let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.Read () |> char
}

let reactPolymer (stack:Stack<Option<char>>) value = 
        match stack.Peek() with
        | Some v when caseInsensitiveEqual v value -> stack.Pop() |> ignore
        | _ -> stack.Push(Some value)

let reactPolymerFolder state value = 
        reactPolymer state value
        state

let filterChar char sequence =
        sequence 
        |> Seq.filter (fun x -> x <> char) 
        |> Seq.filter (fun x -> x <> System.Char.ToUpper char)

let data = readFile "2018-05" 
        |> List.ofSeq

let countWhenCharSkipped char = 
        data
        |> Seq.filter (fun c -> c <> char )
        |> Seq.filter (fun c -> c <> System.Char.ToUpper char)
        |> Seq.fold reactPolymerFolder (makeStack()) |> stackCount

countWhenCharSkipped '-' |> printfn "Nothing skipped %i"

[for c in 'a'..'z' -> (countWhenCharSkipped c, c)] 
|> Seq.min |> printfn "Smallest with char removed %A"
