let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let parseDataLine (dataLine:System.String) =
        dataLine.Split [|','; ' '|]
        |> Array.filter (fun item -> not (item.Equals("")))
        |> Array.map int

let dataPoints =
        readFile "2018-06"
        |> Seq.map parseDataLine
        |> List.ofSeq

let maxDimension (points:List<'a[]>) (d:int) =
        points
        |> Seq.map (fun i -> i.[d])
        |> Seq.max

let maxCoords (coords:List<'a[]>) =
     seq {0 .. coords.[0].Length - 1}
     |> Seq.map (maxDimension coords)

let gridSize =
        maxCoords dataPoints |> List.ofSeq

// Walk 0 -> maxCoords and calculate sizes
