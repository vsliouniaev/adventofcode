// https://adventofcode.com/2018/day/9
open System.Text.RegularExpressions

let readFileLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do yield sr.ReadLine ()
}

let rex = new Regex(@"position=<\s?([-\d]+),\s+?([-\d]+)> velocity=<\s?([-\d]+),\s+?([-\d]+)", RegexOptions.Compiled)
let parse s = 
    let g = (rex.Match s).Groups
    (g.[1].Value |> int,
     g.[2].Value |> int,
     g.[3].Value |> int,
     g.[4].Value |> int)

let maxX points = points |> Seq.map fst |> Seq.max
let maxY points = points |> Seq.map snd |> Seq.max
let minX points = points |> Seq.map fst |> Seq.min
let minY points = points |> Seq.map snd |> Seq.min

let gridSize points =
    let size = maxX points - minX points * maxY points - minY points
    size

let getPoints data = data |> Seq.map (fun (x,y,_,_) -> (x,y)) 

let printGrid data =
    let points = getPoints data
    let pthash = set points
    [for y in minY points..maxY points ->
        [for x in minX points..maxX points -> 
            if Set.contains (x,y) pthash 
            then printf "🌟"
            else printf " " 
        ] |> ignore
        printfn ""
    ]

let moveby n (x,y,i,j) =
    (x + (n * i), 
     y + (n * j), 
     i, j)

let move d = d |> moveby 1

let resetCoords data = 
    let minx = data |> getPoints |> minX 
    let miny = data |> getPoints |> minY
    data |> Seq.map (fun (x,y,i,j) -> (x-minx, y-miny, i, j)) |> List.ofSeq

let simulate jumpBy dat =
    let rec doSim best minSize iterations dat =
        let newPositions = dat |> Seq.map move |> resetCoords
        let thisSize = newPositions |> getPoints |> gridSize
        //printfn "iteration: %i grid size %i" (iterations + jumpBy) minSize
        match thisSize >= minSize with
            | false -> newPositions |> doSim newPositions thisSize (iterations + 1)
            | true -> best
    
    dat |> Seq.map (moveby jumpBy) |> resetCoords |> doSim [] 2147483647 0

readFileLines "2018-10" |> Seq.map parse |> simulate 10000 |> printGrid