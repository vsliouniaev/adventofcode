let readFile (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do yield sr.ReadLine ()
}

type pos = { y:int; x:int; }

type orientation = Up | Right | Down | Left

type trackDirection = Straight | Fslash | Bslash | Intersection

type track = { pos:pos; direction:trackDirection }

type direction = Left | Right | Straight

type cart = { pos:pos; orientation:orientation; nextTurn:direction; }

type location = {track:track; cart:Option<cart>}

let parse x y (c:char) =
    let pos = { x=x; y=y }
    match c with
    | '>' -> Some { cart  = Some { pos=pos; orientation=orientation.Right;nextTurn=direction.Left }; 
                    track =      { pos=pos; direction=trackDirection.Straight }}
    | '<' -> Some { cart  = Some { pos=pos; orientation=orientation.Left;nextTurn=direction.Left  };
                    track =      { pos=pos; direction=trackDirection.Straight }}
    | '^' -> Some { cart  = Some { pos=pos; orientation=orientation.Up;nextTurn=direction.Left    };
                    track =      { pos=pos; direction=trackDirection.Straight }}
    | 'v' -> Some { cart  = Some { pos=pos; orientation=orientation.Down;nextTurn=direction.Left  };
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

let withIndex (seq':seq<'a>) = seq'|>Seq.mapi (fun idx a' -> (idx, a'))

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
    match (track.direction, cart.orientation, cart.nextTurn) with
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
  cart

let getCrashes (oldCartPos:list<cart>) (newCartPos:list<cart>) =
  let pOld = oldCartPos |> List.map (fun c -> c.pos)
  let pNew = newCartPos |> List.map (fun c -> c.pos)
  pNew |> List.filter (fun i -> pOld |> List.contains i)

let rec simUntilFirstCrash (map:Map<pos,track>) (carts:list<cart>)  =
  let updCarts = carts |> List.sortBy (fun c -> c.pos) |> List.map (moveCart map)
  match getCrashes carts updCarts with
  | [] -> simUntilFirstCrash map updCarts
  | crashes  -> crashes

let (carts, map) =
  seq {
    for (y, line)       in readFile "2018-13" |> withIndex do
    for (x, charachter) in line               |> withIndex do
    let it = parse x y charachter
    if it.IsSome then yield it.Value
  } |> Seq.fold toCartsAndMap (List.empty, Map.empty)

let part1 = simUntilFirstCrash map carts //43,91