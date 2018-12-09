// https://adventofcode.com/2018/day/9
open System.Text.RegularExpressions

let readFileLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do yield sr.ReadLine ()
}

let rex = new Regex(@"(\d+)([^\d]+)(\d+)", RegexOptions.Compiled)
let parse s = 
    let g = (rex.Match s).Groups
    (int g.[1].Value, int g.[3].Value)

type Marble(value:bigint) =
    member this.Value : bigint = value
    [<DefaultValue>] val mutable Right : Marble
    [<DefaultValue>] val mutable Left : Marble

// Left
let addRight (newMarble:Marble) (current:Marble) = 
    let c = current
    newMarble.Right <- current.Right
    newMarble.Left <- current
    current.Right.Left <- newMarble
    current.Right <- newMarble
    newMarble

let remove (marble:Marble) =
    let left = marble.Left
    let right = marble.Right
    left.Right <- marble.Right
    right.Left <- marble.Left
    marble.Right // The marble located immediately clockwise of the marble that was removed becomes the new current marble
    // Since it's a circle, there is always something there - skip the null checks

let move (dir: Marble -> Marble) n (marble:Marble) =
    let rec mv c n (m:Marble) =
        match c with
        | _ when c = n -> m
        | _ -> mv (c + 1) n (dir m)
    mv 0 n marble

let moveLeft n marble =
    move (fun m -> m.Left) n marble

let moveRight n marble = 
    move (fun m -> m.Right) n marble

let add (value:bigint) currentMarble =
    // placing the lowest-numbered remaining marble into the circle between the marbles that are 1 and 2 marbles clockwise of the current marble. 
    // (When the circle is large enough, this means that there is one marble between the marble that was just addd and the current marble.) 
    // The marble that was just addd then becomes the current marble.
    currentMarble |> moveRight 1 |> addRight (Marble value)

let playerForNumPlayersFromValue (numPlayers:int) (c:bigint) = 
    let numPlayers = bigint numPlayers
    if c <= numPlayers 
    then 
        int c 
    else 
        int (c % numPlayers)

let displayForNumPlayers (numPlayers:int) (m:Marble) = 
    let mutable c = m
    // printf "[%i] (%i-%i-%i)" (playerForNumPlayersFromValue numPlayers c.Value) c.Left.Value c.Value c.Right.Value
    printf "[%A] (%A)" (playerForNumPlayersFromValue numPlayers c.Value) c.Value
    c <- moveRight 1 c
    while c <> m do
        // printf "  %i-%i-%i" c.Left.Value c.Value c.Right.Value
        printf " %A" c.Value
        c <- moveRight 1 c
    printfn ""
    m

let t = System.Diagnostics.Stopwatch.StartNew()

let data = readFileLines "2018-09" |> Seq.head |> parse
let numPlayers = fst data
let max  = snd data

let display = displayForNumPlayers numPlayers
let playerForValue = playerForNumPlayersFromValue numPlayers

let (elfPoints:bigint[]) = Array.zeroCreate (numPlayers + 1)

let andNowForSomethingCompletelyDifferent (value:bigint) (currentMarble:Marble) = 
    // First, the current player keeps the marble they would have placed, adding it to their score.
    let currentElf = playerForValue value
    elfPoints.[currentElf] <- elfPoints.[currentElf] + value
    
    // In addition, the marble 7 marbles counter-clockwise from the current marble is removed from the circle and also added to the current player's score. 
    let sev = moveLeft 7 currentMarble
    elfPoints.[currentElf] <- elfPoints.[currentElf] + sev.Value
    remove sev

let place (value:bigint) (currentMarble:Marble) =
    let t3 = bigint 23
    let z = bigint 0
    match value with
    | v when v % t3 <> z -> add value currentMarble
    | v when v % t3 =  z -> 
        andNowForSomethingCompletelyDifferent v currentMarble
    | _ -> failwith "Math is broken"

let zero = bigint 0 |> Marble 
zero.Left <- zero
zero.Right <- zero
let mutable current = zero
display current

[for m in 1..(max*100) -> (current <- place (bigint m) current)]
let ans = elfPoints |> Array.max
let took = t.Elapsed
// Part 1 385820

//Part 2, when max2 = max * 100 - 3156297594