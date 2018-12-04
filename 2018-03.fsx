let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

// Sample input: `#1 @ 527,351: 24x10`
let parseDataLine (dataLine:string) = 
    let dat = dataLine.Split [|'@'; ' '; ','; ':'; 'x'|]
    (dat.[3] |> int, 
     dat.[4] |> int, 
     dat.[6] |> int, 
     dat.[7] |> int)

let topLeftCornerAndRectangleSizeToCoordinates (left,  top,  width,  height) =
    seq {left..left+width-1} |> Seq.collect (fun x -> 
    seq {top..top+height-1}  |> Seq.map (fun y -> 
    (x, y)))

let coordinatesToZeroPaddedFormat (x, y) = sprintf "%03i%03i" x y

let visitCoordinatesTrackVisits map coordinates =
        let n = match Map.tryFind coordinates map with
                | Some x -> x + 1
                | None -> 1
        Map.add coordinates n map        

let greaterThanOneFolder state key value =
        if value > 1 then state + 1 else state

readFile "2018-03" 
|> Seq.map parseDataLine
|> Seq.collect topLeftCornerAndRectangleSizeToCoordinates
|> Seq.map coordinatesToZeroPaddedFormat
|> Seq.fold visitCoordinatesTrackVisits Map.empty
|> Map.fold greaterThanOneFolder 0