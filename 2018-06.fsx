let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do yield sr.ReadLine ()
}
let parseDataLine (dataLine:string) =
        dataLine.Split [|','; ' '|] |> Array.filter (fun item -> item <> "")
        |> Array.map int |> List.ofArray
let dataPoints = readFile "2018-06" |> Seq.map parseDataLine |> List.ofSeq
let maxDimension (points:list<list<'a>>) (d:int) = points |> Seq.map (fun i -> i.[d]) |> Seq.max
let minDimension (points:list<list<'a>>) (d:int) = points |> Seq.map (fun i -> i.[d]) |> Seq.min
let maxCoords (coords:list<list<'a>>) = [ for x in 0 .. coords.[0].Length - 1 -> maxDimension coords x ]
let minCoords (coords:list<list<'a>>) = [ for x in 0 .. coords.[0].Length - 1 -> minDimension coords x ]
let gridMax = maxCoords dataPoints
let gridMin = minCoords dataPoints

type PointDistance = { distance:int; idx:int; point:list<int>}
let distance p = p.distance
let pointIdx p = p.idx
let dist (a:list<int>) (b:list<int>) =
        Seq.fold2 (fun (distance,_) a' b' -> distance + abs(a' - b'), a) (0,[]) a b // Fold 2 sets of point coordinates into taxicab/manhattan distance


// let sr = new System.IO.StreamWriter ("fail5")
let mutable skipped = 0
let closestTo (points:list<list<int>>) (currentPoint:list<int>) = 
        let p = points |> Seq.map (dist currentPoint)                                      // Calc distance to all data points for the current grid point 
                |> Seq.mapi (fun idx (distance, point) -> 
                        { idx=idx; distance=distance; point=point })                       // Map to data structure
                |> List.ofSeq
                |> List.sortBy distance                                                    // Use list to leverage pattern-matching
        if p.[0].distance = p.[1].distance
        then 
                skipped <- skipped + 1
                None
        else
                // if p.[0].idx = 'm' then sr.WriteLine p.[0].point
                Some p.[0]
        // |> (fun list -> 
        //         match list with
        //         | [f] -> Some f
        //         | f::s when f.distance <> s.Head.distance -> Some f                // Only return data if it's the closest to exactly one coodrinate
        //         | f::s when f.distance =  s.Head.distance -> None
        //         | _ -> failwithf "Unknown list %A" list )

// closestTo [[1;1];[1;6];[8;3];[3;4];[5;5];[8;9]] [5;2]

[for x in gridMin.[0]..gridMax.[0] -> [for y in gridMin.[1]..gridMax.[1] -> [x; y]]] |> Seq.collect id  // Create grid based on max coordinates of given points
|> Seq.map (closestTo dataPoints) |> Seq.choose id                                            // At each point calc the closest data points
|> Seq.groupBy pointIdx                                                                       // Group by the data point index  
//|> List.ofSeq |> List.iter (fun s -> snd s |> Seq.iter (printfn "%A") )
|> Seq.map (fun (idx, seq') -> (idx, Seq.length seq'))                      // Map index -> char length, data points -> their length 
|> List.ofSeq
|> Seq.sortByDescending snd
// |> Seq.sumBy snd
|> List.ofSeq 
|> List.head
// |> (fun (_,_,s)-> s |> Seq.map (fun f -> f.point) |> Seq.iter (printfn "%A"))

// 9899
// b - 6717

// int 'A'