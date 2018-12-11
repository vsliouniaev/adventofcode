open System.Collections.Generic
open System.Collections.Generic
open System.Collections.Generic
let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do yield sr.ReadLine ()
}

let parseDataLine (s:string) =
        (s.[5], s.[36])

let raw = readFile "2018-07" |> Seq.map parseDataLine |> List.ofSeq
let first = raw |> Seq.map fst
let second = raw |> Seq.map snd
let firstStep = first |> Seq.filter (fun s -> second |> (Seq.contains s) |> not) |> List.ofSeq |> List.distinct

// This data structure is nonsense - fix
let toprereqDict (steps:seq<(char*char)>) =
    steps 
    |> Seq.groupBy snd 
    |> Seq.fold (fun (state:list<char * list<char>>) (k, v) -> 
       state@[(k, (v |> List.ofSeq |> List.map fst))]
    ) []

let prereq = raw |> toprereqDict

let stepTime c = 
   (int c) - 4

let rec doStep (availableSteps:list<char>) (completed:list<char>) (prereq:list<(char * list<char>)>) totalTime =
    let sorted = availableSteps |> List.filter (fun i -> List.contains i completed |> not) |> List.sort 
    let longestTime = sorted |> List.tryLast
    printfn "%A" availableSteps.Length
    match longestTime with
    | None ->
        completed
    | Some c ->
        let totalTime = totalTime + (stepTime c)
        let newPrereq = prereq |> List.map (fun (a,prereq) -> (a,prereq |> List.filter ((<>)c)))
        let unlockedSteps = newPrereq |> List.filter (fun (_,prereq) -> prereq = List.empty ) |> List.map fst
        let newPrereq = newPrereq |> List.filter (fun (_,prereq) -> prereq <> List.empty)
        let newCompleted = completed@[c]
        let newAvail = (availableSteps |> List.filter ((<>) c))@unlockedSteps
        doStep newAvail newCompleted newPrereq totalTime
    
new string [|for c in doStep firstStep [] prereq -> c|]