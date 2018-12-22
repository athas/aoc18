-- This one was OK.  My stencil abuse was not as harmful as it usually
-- is.
--
-- This uses a function 'generate_cave' to produce the cave map for
-- part 2 ('generate_cave' should be run sequentially, but part 2
-- itself is fine in parallel).

-- assumes w < h
let risks (depth: i32) (w: i32) (h: i32) =
  let erosion index = (index + depth) % 20183
  let gindices = replicate h (replicate w depth)
  let gindices[0] = map (*16807) (0..<w)
  let gindices[:,0] = map (*48271) (0..<h)
  let gindices =
    loop gindices for y in 1..<h do
    loop gindices for x in 1..<w do
    gindices with [y,x] = erosion gindices[y-1,x] * erosion gindices[y,x-1]
  in map (map (erosion >-> (%3))) gindices

entry part1 (depth: i32) (x: i32) (y: i32) =
  let risks = risks depth (x+1) (y+1)
  let risks[y,x] = 0
  in risks |> flatten |> i32.sum

entry generate_cave (depth: i32) (x: i32) (y: i32) =
  let risks = risks depth (x+10) (y+10)
  let risks[y,x] = 0
  in risks

type time = { torch: i32
            , gear: i32
            , neither: i32 }

let not_reached: time = { torch = i32.highest/2
                        , gear = i32.highest/2
                        , neither = i32.highest/2 }

let move (from: time, fromkind: i32) (tokind: i32): time =
  match (fromkind, tokind)
  -- rocky->rocky
  case (0, 0) ->
    {torch = from.torch + 1 `i32.min` from.gear + 8,
     gear = from.gear + 1 `i32.min` from.torch + 8,
     neither = not_reached.neither }
  -- rocky->wet
  case (0, 1) ->
    {torch = not_reached.torch,
     gear = from.gear + 1 `i32.min` from.torch + 8,
     neither = not_reached.neither }
  -- rocky->narrow
  case (0, _) ->
    {torch = from.torch + 1 `i32.min` from.gear + 8,
     gear = not_reached.gear,
     neither = not_reached.neither }

  -- wet->rocky
  case (1, 0) ->
    {torch = not_reached.torch,
     gear = from.neither + 8 `i32.min` from.gear + 1,
     neither = not_reached.neither }
  -- wet->wet
  case (1, 1) ->
    {torch = not_reached.torch,
     gear = from.gear + 1 `i32.min` from.torch + 8,
     neither = from.neither + 1 `i32.min` from.gear + 8 }
  -- wet->narrow
  case (1, 2) ->
    {torch = not_reached.torch,
     gear = not_reached.gear,
     neither = from.neither + 1 `i32.min` from.gear + 8 }

  -- narrow->rocky
  case (_, 0) ->
    {torch = from.torch + 1 `i32.min` from.neither + 8,
     gear = not_reached.neither,
     neither = not_reached.neither }
  -- narrow->wet
  case (_, 1) ->
    {torch = not_reached.gear,
     gear = not_reached.gear,
     neither = from.torch + 8 `i32.min` from.neither + 1 }
  -- narrow->narrow
  case (_, 2) ->
    {torch = from.torch + 1 `i32.min` from.neither + 8,
     gear = not_reached.gear,
     neither = from.neither + 1 `i32.min` from.gear + 8 }

let tmin (t1: time) (t2: time): time =
  { torch = t1.torch `i32.min` t2.torch,
    gear = t1.gear `i32.min` t2.gear,
    neither = t1.neither `i32.min` t2.neither }

let step [h][w] (cave: [h][w]i32) (times: [h][w]time): [h][w]time =
  let get x y =
    if x >= 0 && x < h && y >= 0 && y < w
    then unsafe (times[x,y], cave[x,y]) else (not_reached, 0)
  let evolve x y =
    let (n, s, w, e, (c, kind)) = (get (x-1) y, get (x+1) y,
                                   get x (y-1), get x (y+1),
                                   get x y)
    in move n kind `tmin` move s kind `tmin` move w kind `tmin` move e kind `tmin` c

  in tabulate_2d h w evolve

-- answer: 952

entry part2 [h][w] (cave: [h][w]i32) (y: i32) (x: i32) =
  let times = replicate h (replicate w not_reached)
  let times[0,0] = {torch=0, gear=7, neither=7}
  let continue = true
  let (times, _) = loop (times, continue) while continue do
                   let new_times = step cave times
                   in (new_times, times != new_times)
  let {torch, gear, neither} = times[x,y]
  in (torch-1, gear+7, neither)

