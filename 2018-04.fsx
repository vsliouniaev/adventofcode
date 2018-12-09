open System
open System.Text.RegularExpressions
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
           // TODO: Make this tail-recursive
        split [] -1 list    

let totalSleepReducer acc ((f:Event), (s:Event)) =
        acc + match (f,s) with
              | (f, s) when f.kind = State.sleep && s.kind = State.wake -> (s.time - f.time).TotalMinutes |> int
              | _ -> 0

let guardSleptFor seq' =
        Seq.pairwise seq' 
        |> Seq.fold totalSleepReducer 0

let data = readFile "2018-04" |> Seq.sort |> List.ofSeq |> splitGuard |> List.groupBy (fun ev -> ev.guard)

let sleepiest data =
        let mutable s = 0
        let mutable maxSleep = 0
        let mutable sleepiestMin = -1        
        data 
        |> Seq.map (fun ev -> (ev.time.Minute, ev.kind)) 
        |> Seq.sort
        |> Seq.iter (fun ev -> // TODO: Use a fold 
                match snd ev with
                | State.sleep -> 
                        s <- s + 1
                        if s > maxSleep then maxSleep <- s; sleepiestMin <- fst ev
                | State.wake -> s <- s - 1
                | _ -> ())
        (maxSleep, sleepiestMin)               
let sleepiestGuard = data |> Seq.map (fun item -> fst item, (item |> snd |> guardSleptFor)) |> Seq.maxBy snd
let sleepiestGuardData = data |> Seq.find (fun f -> fst sleepiestGuard = fst f) |> snd // This probably has a single function
let part1 = sleepiestGuardData |> sleepiest |> snd |> (*) (fst sleepiestGuard) // 21956

let part2 = data |> Seq.map (fun g -> 
        let depthAndMin = snd g |> sleepiest
        (fst depthAndMin, fst g * (snd depthAndMin))) |> Seq.max |> snd // 124511
