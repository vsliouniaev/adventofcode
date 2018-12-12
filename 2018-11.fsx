
let cellLevel serial x y =
    let rackid = x + 10
    let power = rackid * y
    let power = power + serial
    let power = power * rackid
    let power = power / 100 % 10
    let power = power - 5
    power

let coords x y = sprintf "%3i,%3i" x y

let addMap (map:Map<string, int>) (pow, (x, y)) =
    map.Add((coords x y), pow)

let blockPower gid (statex, statey, statesz, statepow) (size, (x,y)) =
    // todo - limit to grid - ?
    let thispow = seq { for x' in x..x+size do 
                        for y' in y..y+size do 
                        yield cellLevel gid x' y' } |> Seq.sum
    if thispow > statepow
    then
        printfn "%i,%i,%i %i" x y (size + 1) thispow 
        (x, y, (size + 1), thispow)
    else 
        (statex, statey, statesz, statepow)

let power3x3 serial x y = 
    seq { for x' in x..x+2 do 
          for y' in y..y+2 do 
          yield cellLevel serial x' y' }|> Seq.sum

let gid = 7403
let part1 = 
    seq { for x in 1..301 do
          for y in 1..301 do
          yield (power3x3 gid x y, (x, y)) } |> Seq.max
// 235,48
let limit = 20
let part2 = 
    seq { for pow in 1..limit do
          for x in 1..301 do
          for y in 1..301 do
          yield (pow, (x,y)) } |> Seq.fold (blockPower gid) (0,0,0,0)

// 285,113,11 89