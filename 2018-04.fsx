// Parsing
open System
open System.Text.RegularExpressions
type GuardState = start = 0 | sleep = 1 | wake = 2
type Event = { guard:int; time:DateTime; kind:GuardState }
let guardIdRex = new Regex(@"#(\d+) ", RegexOptions.Compiled)
let parseGuardId s = (guardIdRex.Match s).Groups.[1].Value |> int
let parseTime (s:string) = s.[1..16] |> DateTime.Parse
type SleepInfo = { current:int; deepest:int; minute:int } 
        with static member Default = { current=0; deepest=0; minute=0 }

type SleepEvent = { minute:int; kind:GuardState }

// If there is no guard id in the event, take the id from the previous event with one
let parseEvent setGuardId (dataString:string) =
        let kind = match dataString.[dataString.Length - 2] with
                   | c when c = 'f' -> GuardState.start
                   | c when c = 'e' -> GuardState.sleep
                   | c when c = 'u' -> GuardState.wake
                   | _ -> failwithf "Can't parse data: %s" dataString
        let guard = if kind <> GuardState.start then setGuardId else parseGuardId dataString
        { kind = kind; time = parseTime dataString; guard = guard }

// Funcs
let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do yield sr.ReadLine ()
}

let splitEventsByGuards list = 
        let rec split (state:list<Event>) lastGuard list =
          match list with
          | [] -> state
          | head :: tail ->
                let ev = parseEvent lastGuard head
                List.concat[[ev]; split state ev.guard tail;]
           // TODO: Make this tail-recursive
        split [] -1 list    

let sleepDurationFromEventPair state ((first:Event), (second:Event)) =
        state + match (first,second) with
              | (f, s) 
                when f.kind = GuardState.sleep && s.kind = GuardState.wake -> 
                (s.time - f.time).TotalMinutes |> int
              | _ -> 0

let deepestSleep state data =
                match data.kind with
                | GuardState.sleep -> 
                        let current = state.current + 1
                        {               
                            current = current
                            deepest = if current > state.deepest then current else state.deepest
                            minute  = if current > state.deepest then data.minute else state.minute 
                        }
                | GuardState.wake -> 
                        { state with current = state.current - 1 }
                | _ -> state
let sleepiest data = 
        data 
        |> Seq.map (fun ev -> { minute=ev.time.Minute; kind=ev.kind })
        |> Seq.sortBy (fun ev -> ev.minute)
        |> Seq.fold deepestSleep SleepInfo.Default 

let guardSleptFor seq' = Seq.pairwise seq' |> Seq.fold sleepDurationFromEventPair 0

// Solve
let data = readFile "2018-04" |> Seq.sort |> List.ofSeq |> splitEventsByGuards |> List.groupBy (fun ev -> ev.guard)   
let sleepiestGuard = data |> Seq.map (fun item -> fst item, (item |> snd |> guardSleptFor)) |> Seq.maxBy snd
let sleepiestGuardData = data |> Seq.find (fun f -> fst sleepiestGuard = fst f) |> snd
let part1 = sleepiestGuardData |> sleepiest |> (fun f -> f.minute ) |> (*) (fst sleepiestGuard) // 21956

let part2 = data |> Seq.map (fun g -> 
        let depthAndMin = snd g |> sleepiest
        (depthAndMin.deepest, fst g, fst g * (depthAndMin.minute))) |> Seq.max // 134511
