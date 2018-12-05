// Part 1
let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

readFile "2018-01"
|> Seq.map int
|> Seq.fold (+) 0
|> printfn "%i"

// Part 2

let mutable keepGoing = true
let frequencies = new System.Collections.Generic.HashSet<int>()

let readFileContinuously (filePath:string) = seq {
    let loopStreamReader (sr:System.IO.StreamReader) = 
        if sr.EndOfStream then
            sr.DiscardBufferedData()
            sr.BaseStream.Seek(int64(0), System.IO.SeekOrigin.Begin) |> ignore
    use sr = new System.IO.StreamReader (filePath)
    while keepGoing do
        loopStreamReader sr
        yield sr.ReadLine ()
}

let checkFrequency (frequencies:System.Collections.Generic.HashSet<int>) frequency = frequencies.Add(frequency)

let folder state value =
    let added = value + state
    keepGoing <- checkFrequency frequencies added
    if not keepGoing then printfn "%i" added
    added    
    
readFileContinuously "2018-01"
|> Seq.map int
|> Seq.fold folder 0