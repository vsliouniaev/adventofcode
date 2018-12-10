let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do yield sr.ReadLine ()
}
let parseDataLine (dataLine:string) =
        dataLine.Split [|','; ' '|] |> Array.filter (fun item -> item <> "")
        |> Array.map int |> List.ofArray
let dataPoints = readFile "2018-06" |> Seq.map parseDataLine |> List.ofSeq
let maxDimension (points:seq<list<'a>>) (d:int) = points |> Seq.map (fun i -> i.[d]) |> Seq.max
let minDimension (points:seq<list<'a>>) (d:int) = points |> Seq.map (fun i -> i.[d]) |> Seq.min
let maxCoords (coords:list<list<'a>>) = [ for x in 0 .. coords.[0].Length - 1 -> maxDimension coords x ]
let minCoords (coords:list<list<'a>>) = [ for x in 0 .. coords.[0].Length - 1 -> minDimension coords x ]
let gridMax = maxCoords dataPoints
let gridMin = minCoords dataPoints

type PointDistance = { distance:int; idx:int; point:list<int>}
let distance p = p.distance
let pointIdx p = p.idx
let dist (a:list<int>) (b:list<int>) =
        Seq.fold2 (fun (distance,_) a' b' -> distance + abs(a' - b'), a) (0,[]) a b // Fold 2 sets of point coordinates into taxicab/manhattan distance


let closestTo (points:seq<list<int>>) (currentPoint:list<int>) = 
        let p = points |> Seq.map (dist currentPoint)                                      
                |> Seq.mapi (fun idx (distance, point) -> { idx=idx; distance=distance; point=point })                      
                |> Seq.sortBy distance |> Seq.take 2 |> List.ofSeq
        if p.[0].distance <> p.[1].distance
        then Some p.[0]
        else None

let touchesGridEdge (max:list<int>) (min:list<int>) (point:list<int>) = 
    let touches = Seq.fold2 (fun state point' coord' -> state || point' = coord') false point
    touches max || touches min

let filterTouchingEdge (_,closestCoords) = 
    let touches = closestCoords |> Seq.map (fun p -> p.point) |> Seq.exists (touchesGridEdge gridMax gridMin)
    touches |> not

[for x in gridMin.[0]..gridMax.[0] -> [for y in gridMin.[1]..gridMax.[1] -> [x; y]]] |> Seq.collect id
|> Seq.map (closestTo dataPoints) |> Seq.choose id
|> Seq.groupBy pointIdx                           
|> Seq.filter filterTouchingEdge          
|> Seq.map (fun (idx, seq') -> (idx, Seq.length seq', seq'))
|> Seq.sortByDescending (fun (_,x,_) -> x)
|> List.ofSeq |> List.head