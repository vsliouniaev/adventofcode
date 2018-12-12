let readFileLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do yield sr.ReadLine ()
}

let parsePlants (s:string) = s.[15..s.Length-1] |> Array.ofSeq
let parseRule (s:string) = (s.[0..4], s.[9])

let parseData data = 
    let plants = Seq.head data |> parsePlants
    let rules  = Seq.skip 2 data |> Seq.map parseRule |> Map.ofSeq
    (plants, rules)

let data = readFileLines "2018-12" |> parseData
let plants = fst data
let rules  = snd data

let pad plants sz = 
    let pad = Array.create sz '.'
    Array.append (Array.append pad plants) pad

let countPlants (plants,offset) = 
    plants |> Seq.mapi (fun idx c -> if c = '#' then idx - offset else 0) |> Seq.sum

let nextGen (p, offset0) rules =
    let window = rules |> Map.toSeq |> Seq.head |> fst |> String.length
    let i = Seq.windowed window (pad p window) 
            |> Seq.map (fun charray -> new string (charray))
            |> Seq.map (fun s -> match rules.TryFind s with | Some v -> v | None -> '.') 
            |> Array.ofSeq
    (i, offset0 + (window / 2) + 1)


let simulate plants rules (maxIterations) =
    let rec sim maxIterations data prevCount iteration delta =
        let data = nextGen data rules
        let count = countPlants data
        // if iteration % 1000 = 0 then printfn "%A - %A" iteration count
        let newDelta = count - prevCount
        if iteration = maxIterations || delta = newDelta
        then (count, iteration, delta)
        else sim maxIterations data count (iteration + 1) newDelta
    sim maxIterations (plants, 0) 0 1 0

let part1 = simulate plants rules 20 //1917

//1250000000991
let part2 = simulate plants rules 500000000 
            |> (fun (c, i, d) -> (50000000000UL - (uint64 i)) * (uint64 d) + (uint64 c) )  