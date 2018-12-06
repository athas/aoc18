-- Cellular automata!
--
-- Each cell value is the index of the closest point.
--
-- We use -1 to represent 'undetermined' and -2 to represent 'multiple equally distant'.

type pos = {x:i32, y:i32}

let distance (p1: pos) (p2: pos): i32 =
  i32.abs(p1.x-p2.x) + i32.abs(p1.y-p2.y)

let parse: [][2]i32 -> []pos = map (\r -> {x=r[0], y=r[1]})

let check_all_points [num_points] (points: [num_points]pos) (h: i32) (w: i32): [h][w]i32 =
  let f (x: (i32, i32)) (y: (i32, i32)) =
    if x.1 == y.1 then (x.1, -2)
    else if x.1 < y.1 then x else y
  let on_point x y = reduce f (h*w, -1) (zip (map (distance {x,y}) points) (iota num_points))
  in tabulate_2d h w on_point |> map (map (.2))

let histogram [n] (k: i32) (xs: [n]i32): [k]i32 =
  reduce_by_index (replicate k 0) (+) 0 xs (replicate n 1)

let count 't (f: t -> bool) (ts: []t): i32 =
  map f ts |> map i32.bool |> i32.sum

entry part1 [num_points] (input: [num_points][2]i32) =
  let points = parse input
  let max_x = points |> map (.x) |> i32.maximum |> (+1)
  let max_y = points |> map (.y) |> i32.maximum |> (+1)
  let grid = check_all_points points max_x max_y
  let edge = grid[0] ++ grid[max_x-1] ++ grid[:,0] ++ grid[:,max_y-1]
  let on_edge i = any (==i) edge
  let area_per_point = flatten grid |> histogram num_points
  in pick (map on_edge (iota num_points)) (replicate num_points 0) area_per_point
     |> i32.maximum

entry part2 [num_points] (input: [num_points][2]i32) =
  let points = parse input
  let max_x = points |> map (.x) |> i32.maximum |> (+1)
  let max_y = points |> map (.y) |> i32.maximum |> (+1)
  let f x y = map (distance {x,y}) points |> i32.sum
  let limit = 10000
  in tabulate_2d max_x max_y f |> flatten |> count (<limit)
