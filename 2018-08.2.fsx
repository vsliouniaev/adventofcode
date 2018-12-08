let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let parseLine (s:string) = 
        s.Split [|' '|]
        |> Seq.map int

let calcMetaSum (nodeSums:int[]) metaVal =
        if metaVal >= 1 && metaVal <= nodeSums.Length  then
                nodeSums.[metaVal - 1] 
        else 0

let rec readNode (data:int[]) (startIdx:int) =
        let nodes = data.[startIdx]
        let metas = data.[startIdx + 1]
        let mutable nodeStart = startIdx + 2
        // printfn "%i-nodes: %i" startIdx nodes
        // printfn "%i-metas: %i" startIdx metas
        let nodeSums = Array.create nodes 0
        if nodes > 0 then
                for i in seq {0..nodes - 1} do
                        let nodeDat = readNode data nodeStart
                        nodeStart <- fst nodeDat
                        nodeSums.[i] <- snd nodeDat
        
        let metaStart = nodeStart
        // printfn "%i-metaStart %i" startIdx metaStart
        let mutable sum = 0
        if metas > 0 then
                for metaIdx in seq {metaStart..metaStart + metas - 1} do
                        let metaVal = data.[metaIdx]
                        let metaSum = calcMetaSum nodeSums metaVal
                        sum <- sum + metaSum
                        if nodes = 0 then 
                                sum <- sum + metaVal
                        // printfn "%i-metaval: %i" startIdx metaVal
                                
        (nodeStart + metas, sum)

let data = readFile "2018-08"
        |> Seq.map parseLine
        |> Seq.head 
        |> Array.ofSeq

readNode data 0 //21405