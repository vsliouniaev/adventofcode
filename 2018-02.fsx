let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

// Part 1

let parseDataLine (s:string) =
    let counts = Array.create 26 0
    s.ToCharArray()
        |> Seq.iter (fun i -> 
            let currentLetterCount = counts.[int(i) - int('a')]
            counts.[int(i) - int('a')] <-  currentLetterCount + 1)
    (Array.contains 3 counts, Array.contains 2 counts)

let folder state value =
    let twos = if fst value then 1 else 0
    let thrs = if snd value then 1 else 0
    (fst state + twos, snd state + thrs)

readFile "2018-02"
|> Seq.map parseDataLine
|> Seq.fold folder (0, 0)
|> (fun (twos, thrs) -> twos * thrs)

// Part 2

let differByOneLetter (l1:char[], l2:char[]) = 
    let f = Array.map2 (fun x y -> if x.Equals(y) then Some x else None ) l1 l2 |> Array.choose id
    if l1.Length.Equals(f.Length + 1) then Some f else None

readFile "2018-02" 
|> Seq.sort
|> Seq.map Seq.toArray
|> Seq.pairwise
|> Seq.map differByOneLetter
|> Seq.choose id
|> Seq.head 
|> System.String.Concat