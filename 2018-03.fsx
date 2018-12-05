let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

// Sample input: `#1 @ 527,351: 24x10`
let parseDataLine (dataLine:string) = 
    let dat = dataLine.Split [|'#'; ' '; ','; ':'; 'x'|]
    (dat.[1] |> int,
     dat.[3] |> int, 
     dat.[4] |> int, 
     dat.[6] |> int, 
     dat.[7] |> int)

let topLeftCornerAndRectangleSizeToCoordinates (i, left,  top,  width,  height) =
    seq {left..left+width-1} |> Seq.collect (fun x -> 
    seq {top..top+height-1}  |> Seq.map (fun y -> 
    (i, x, y)))

let coordinatesToZeroPaddedFormat (i, x, y) = (i, sprintf "%03i%03i" x y)

let visitCoordinatesTrackVisits (map, set) idxAndCoordinates =
        let idx = fst idxAndCoordinates
        let coords = snd idxAndCoordinates
        let (updatedIndexOfFreeSquares, numClashes) = 
                match Map.tryFind coords map with
                | Some (i, x) ->
                        let setOfFreeSquares = Set.add idx <| Set.add i set
                        let numClashes = x + 1
                        (setOfFreeSquares, numClashes)
                | None -> (set, 1)
        let updatedOverlapMap = Map.add coords (idx, numClashes) map
        (updatedOverlapMap, updatedIndexOfFreeSquares)

let greaterThanOneFolder state key value =
        if snd value > 1 then state + 1 else state
let out = readFile "2018-03"
        |> Seq.map parseDataLine
        |> Seq.collect topLeftCornerAndRectangleSizeToCoordinates
        |> Seq.map coordinatesToZeroPaddedFormat
        |> Seq.fold visitCoordinatesTrackVisits (Map.empty, Set.empty)

let overlappingInches =    
        fst out |> Map.fold greaterThanOneFolder 0
let oneAndOnlyFreeSquare = 
        let items = snd out 
        let all = Set.ofSeq [1..items.Count + 1]
        Set.difference all items |> Set.minElement

printfn "Part 1: %i" overlappingInches
printfn "Part 2: %i" oneAndOnlyFreeSquare
