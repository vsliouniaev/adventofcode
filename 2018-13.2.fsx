let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do yield sr.ReadLine ()
}

type pos = { y:int; x:int; }

type orientation = Up | Right | Down | Left

type trackDirection = Straight | Fslash | Bslash | Intersection

type track = { pos:pos; direction:trackDirection }

type direction = Left | Right | Straight

type cart = { pos:pos; orientation:orientation; nextTurn:direction; id:string; moves:int }

type location = {track:track; cart:Option<cart>}

type Circle<'a when 'a : comparison>(items:list<'a>) =
  member private __.Items = items
  static member count (c:Circle<'a>) = c.Items.Length
  static member next (c:Circle<'a>) =
    match c.Items with
    | [single] -> [single]
    | first::rest -> rest@[first]
    | [] -> []
    |> Circle
  
  static member replaceCurrent (a':'a) (c:Circle<'a>) =
    match c.Items with
    | [_] -> [a']
    | _::rest -> [a']@rest 
    | [] -> failwith "invalid trying to replace empty circle"
    |> Circle

  static member current (c:Circle<'a>) =
    List.head c.Items
  static member tryFind f (c:Circle<'a>) =
    c.Items |> List.tryFind f

  static member filterBy f (c:Circle<'a>) =
    c.Items |> List.filter f |> Circle
  
  static member sortBy f (c:Circle<'a>) =
    c.Items |> List.sortBy f |> Circle

let parse x y (c:char) =
    let pos = { x=x; y=y }
    let id = sprintf "%03ix%03iy" pos.x pos.y
    match c with
    | '>' -> Some { cart  = Some { pos=pos; orientation=orientation.Right;nextTurn=direction.Left; id=id; moves=0 }; 
                    track =      { pos=pos; direction=trackDirection.Straight }}
    | '<' -> Some { cart  = Some { pos=pos; orientation=orientation.Left;nextTurn=direction.Left; id=id; moves=0 };
                    track =      { pos=pos; direction=trackDirection.Straight }}
    | '^' -> Some { cart  = Some { pos=pos; orientation=orientation.Up;nextTurn=direction.Left; id=id; moves=0 };
                    track =      { pos=pos; direction=trackDirection.Straight }}
    | 'v' -> Some { cart  = Some { pos=pos; orientation=orientation.Down;nextTurn=direction.Left; id=id; moves=0 };
                    track =      { pos=pos; direction=trackDirection.Straight }}            
    | '-' -> Some { cart  = None; 
                    track = { pos=pos; direction=trackDirection.Straight }}
    | '|' -> Some { cart  = None; 
                    track = { pos=pos; direction=trackDirection.Straight }}
    |'\\' -> Some { cart  = None; 
                     track = { pos=pos; direction=trackDirection.Bslash  }}
    | '/' -> Some { cart  = None; 
                    track = { pos=pos; direction=trackDirection.Fslash   }}
    | '+' -> Some { cart  = None; 
                    track = { pos=pos; direction=trackDirection.Intersection }}          
    | ' ' -> None
    | _ -> failwithf "Found unknown char %c" c

let toCartsAndMap (carts:list<cart>, map:Map<pos, track>) (loc:location) = 
  let carts' = match loc.cart with
               | Some cart -> List.append [cart] carts
               | None -> carts
  let map' = Map.add loc.track.pos loc.track map
  (carts', map')             

let moveCart (map:Map<pos,track>) (cart:cart) = 
  let cart =
    {cart with pos = match cart.orientation with
                     | orientation.Up    -> {x=cart.pos.x  ; y=cart.pos.y-1 }
                     | orientation.Right -> {x=cart.pos.x+1; y=cart.pos.y   }
                     | orientation.Down  -> {x=cart.pos.x  ; y=cart.pos.y+1 }
                     | orientation.Left  -> {x=cart.pos.x-1; y=cart.pos.y   }}
  let track = 
    match Map.tryFind cart.pos map with
    | Some p -> p
    | None -> failwithf "Cannot find map at %A" cart.pos
  let cart =
    match (track.direction, cart.orientation, cart.nextTurn) with // Discriminated unions prevent this from compiling until all cases have been covered
    | (trackDirection.Fslash, orientation.Up   , _) -> { cart with orientation=orientation.Right } // / * ^ -> >
    | (trackDirection.Fslash, orientation.Right, _) -> { cart with orientation=orientation.Up    } // / * > -> ^
    | (trackDirection.Fslash, orientation.Down , _) -> { cart with orientation=orientation.Left  } // / * v -> <
    | (trackDirection.Fslash, orientation.Left , _) -> { cart with orientation=orientation.Down  } // / * < -> v
    | (trackDirection.Bslash, orientation.Up   , _) -> { cart with orientation=orientation.Left  } // \ * ^ -> <
    | (trackDirection.Bslash, orientation.Right, _) -> { cart with orientation=orientation.Down  } // \ * > -> v
    | (trackDirection.Bslash, orientation.Down , _) -> { cart with orientation=orientation.Right } // \ * v -> >
    | (trackDirection.Bslash, orientation.Left , _) -> { cart with orientation=orientation.Up    } // \ * < -> ^
    | (trackDirection.Intersection, orientation.Up    , direction.Left)     -> { cart with orientation=orientation.Left ; nextTurn=direction.Straight } // ^ -> <
    | (trackDirection.Intersection, orientation.Right , direction.Left)     -> { cart with orientation=orientation.Up   ; nextTurn=direction.Straight } // > -> ^
    | (trackDirection.Intersection, orientation.Down  , direction.Left)     -> { cart with orientation=orientation.Right; nextTurn=direction.Straight } // v -> >
    | (trackDirection.Intersection, orientation.Left  , direction.Left)     -> { cart with orientation=orientation.Down ; nextTurn=direction.Straight } // < -> v
    | (trackDirection.Intersection, _                 , direction.Straight) -> { cart with nextTurn=direction.Right     }  
    | (trackDirection.Intersection, orientation.Up    , direction.Right)    -> { cart with orientation=orientation.Right; nextTurn=direction.Left     } // ^ -> >
    | (trackDirection.Intersection, orientation.Right , direction.Right)    -> { cart with orientation=orientation.Down ; nextTurn=direction.Left     } // > -> v
    | (trackDirection.Intersection, orientation.Down  , direction.Right)    -> { cart with orientation=orientation.Left ; nextTurn=direction.Left     } // v -> <
    | (trackDirection.Intersection, orientation.Left  , direction.Right)    -> { cart with orientation=orientation.Up   ; nextTurn=direction.Left     } // < -> ^
    | (trackDirection.Straight, _, _) -> cart
  {cart with moves = cart.moves + 1}

let sortIfLooped tick carts =                                    // The order of processing isn't continuos, instead, every time
  if (carts |> Circle.current).moves > tick                      // all carts have been processed we start again, ordered by y,x
  then (carts |> Circle.sortBy (fun cart -> cart.pos), tick + 1) // defining pos={y;x} makes the default sort order work
  else (carts, tick)                                             

let simCart (map:Map<pos,track>) (carts:Circle<cart>) (tick:int) =
  let (carts, tick) = carts |> sortIfLooped tick                                         
  let moved         = carts |> Circle.current |> moveCart map
  let crashedInto   = carts |> Circle.tryFind (fun item -> item.pos = moved.pos)
  let carts = match crashedInto with
              | None       -> carts
                              |> Circle.replaceCurrent moved |> Circle.next
              | Some crash -> carts 
                              |> Circle.next  // Call first to ensure we handle cases where we remove the immediately-next cart
                              |> Circle.filterBy (fun item -> item.id  <> moved.id ) // Remove the crasher cart
                              |> Circle.filterBy (fun item -> item.pos <> crash.pos) // Remove the crashed-into (crashee?) cart
  (carts, tick, crashedInto)

let rec simUntilFirstCrash (map:Map<pos,track>) (carts:Circle<cart>) (tick:int) =
  let (carts, tick, crash) = simCart map carts tick
  if crash.IsSome 
  then crash.Value
  else simUntilFirstCrash map carts tick
let rec simUntilOneCart (map:Map<pos,track>) (carts:Circle<cart>) (tick:int) =
  let (carts, tick, _) = simCart map carts tick
  if Circle.count carts = 1
  then Circle.current carts
  else simUntilOneCart map carts tick

let (carts, map) =
  let withIndex (seq':seq<'a>) = seq'|>Seq.mapi (fun idx a' -> (idx, a'))
  seq {
    for (y, line)       in readFile "2018-13" |> withIndex do
    for (x, charachter) in line               |> withIndex do
    let it = parse x y charachter
    if it.IsSome then yield it.Value
  } |> Seq.fold toCartsAndMap (List.empty, Map.empty)

let part1 = simUntilFirstCrash map (carts |> Circle) 0 //43,91
let part2 = simUntilOneCart    map (carts |> Circle) 0 //35,59