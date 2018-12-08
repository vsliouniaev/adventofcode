open System.Text.RegularExpressions
open System
open System.Collections.Generic

type State = start = 0 | sleep = 1 | wake = 2
type Event = {guard:int; time:DateTime; kind:State}
let rex = new Regex(@"#(\d+) ", RegexOptions.Compiled)
let parseGuardId s = (rex.Match s).Groups.[1].Value |> int
let parseTime (s:string) = s.[1..16] |> DateTime.Parse
let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do yield sr.ReadLine ()
}

let parseEvent lastGuard (s:string) =
        let kind = match s.[s.Length - 2] with
                   | c when c = 'f' -> State.start
                   | c when c = 'e' -> State.sleep
                   | c when c = 'u' -> State.wake
                   | _ -> failwithf "Unknown string %s" s
        let guard = if kind <> State.start then lastGuard else parseGuardId s
        { kind = kind; time = parseTime s; guard = guard }

let splitGuard list = 
        let rec split (state:list<Event>) lastGuard list =
          match list with
          | [] -> state
          | head :: tail ->
                let ev = parseEvent lastGuard head
                List.concat[[ev]; split state ev.guard tail;]
        split [] -1 list
        // TODO: Is this tail-recursive?       

let totalSleepReducer acc ((f:Event), (s:Event)) =
        acc + match (f,s) with
              | (f, s) when f.kind = State.sleep && s.kind = State.wake -> (s.time - f.time).TotalMinutes |> int
              | _ -> 0


let guardSleptFor seq' =
        Seq.pairwise seq' 
        |> Seq.fold totalSleepReducer 0

let data = readFile "2018-04.1" |> Seq.sort |> List.ofSeq
splitGuard data 
|> Seq.groupBy (fun ev -> ev.guard) 
|> Seq.map (fun item -> fst item, (item |> snd |> guardSleptFor))
|> List.ofSeq