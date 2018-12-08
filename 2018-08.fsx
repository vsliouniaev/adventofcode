let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let parseLine (s:string) = 
        s.Split [|' '|]
        |> Seq.map int

let mutable sum = 0
let rec readNode (data:int[]) (startIdx:int) =
        let nodes = data.[startIdx]
        let metas = data.[startIdx + 1]
        let mutable nodeStart = startIdx + 2
        printfn "%i-nodes: %i" startIdx nodes
        printfn "%i-metas: %i" startIdx metas
        if nodes > 0 then
                for _ in seq {1..nodes} do
                        nodeStart <- readNode data nodeStart
        let metaStart = nodeStart
        printfn "%i-metaStart %i" startIdx metaStart
        if metas > 0 then
                for metaIdx in seq {metaStart..metaStart + metas - 1} do
                        sum <- sum + data.[metaIdx]
                        printfn "%i-metaval: %i" startIdx data.[metaIdx]
                                
        nodeStart + metas

let data = readFile "2018-08"
        |> Seq.map parseLine
        |> Seq.head 
        |> Array.ofSeq

readNode data 0 //46781