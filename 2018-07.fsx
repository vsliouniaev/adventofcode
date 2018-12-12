let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do yield sr.ReadLine () }
let parseDataLine (s:string) = (s.[5], s.[36])
let data = readFile "2018-07" |> Seq.map parseDataLine |> List.ofSeq
let notContainedIn (other:seq<'a>) (a:'a) = Seq.contains a other |> not
let maxParallel = 5
let stepTime c = (int c) - 4

let availableSteps (complete:list<char>) (inProgress:list<(char*int)>) (data:list<(char*char)>) =
   let all = data |> List.unzip |> (fun (l1, l2) -> List.append l1 l2) |> Set.ofList
   let locked = data |> List.filter (fun (r,_) -> notContainedIn complete r ) |> List.map snd |> List.distinct
   let availableSteps = all 
                        |> Seq.filter (notContainedIn locked) 
                        |> Seq.filter (notContainedIn complete) 
                        |> Seq.filter (notContainedIn (Seq.map fst inProgress))
                        |> Seq.sort
   availableSteps |> Seq.map (fun f -> (f, stepTime f)) |> List.ofSeq |> List.sort

let rec go total (completed:list<char>) (inProgress:list<(char*int)>) (data:list<(char*char)>) =
   let tasks   = inProgress @ (availableSteps completed inProgress data)
   if tasks = [] 
      then (total, new string [| for c in completed -> c|])
   else
      let time        = tasks  |> Seq.minBy snd |> snd
      let worked      = tasks  |> List.truncate maxParallel |> List.map (fun (task, duration) -> (task, duration - time))
      let finished    = worked |> List.filter (fun (_, duration) -> duration  = 0 ) |> List.map fst
      let inProgress  = worked |> List.filter (fun (_, duration) -> duration <> 0 )
      go (total+time) (completed@finished) inProgress data

go 0 [] [] data

//      OUGLTKDJVBRMIXSACWYPEQNHZF
// 929, OUXYGLVTRKMBDIJSACPWEQNZHF
