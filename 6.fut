type pos = {x:i32, y:i32}

let distance (p1: pos) (p2: pos): i32 =
  i32.abs(p1.x-p2.x) + i32.abs(p1.y-p2.y)

let parse: [][2]i32 -> []pos = map (\r -> {x=r[0], y=r[1]})

let check_all_points [num_points] (points: [num_points]pos) (h: i64) (w: i64): [h][w]i32 =
  let f x y = if x.0 == y.0 then (x.0, -2)
              else if x.0 < y.0 then x else y
  let on_point x y = reduce_comm f (i32.i64 (h*w), -1)
                                 (zip (map (distance {x=i32.i64 x,y=i32.i64 y}) points)
                                      (map i32.i64 (iota num_points)))
  in tabulate_2d h w on_point |> map (map (.1))

let histogram [n] (k: i64) (xs: [n]i32): [k]i32 =
  reduce_by_index (replicate k 0) (+) 0 (map i64.i32 xs) (replicate n 1)

let count 't (f: t -> bool) (ts: []t): i32 =
  map f ts |> map i32.bool |> i32.sum

entry part1 [num_points] (input: [num_points][2]i32) =
  let points = parse input
  let max_x = points |> map (.x) |> i32.maximum |> (+1)
  let max_y = points |> map (.y) |> i32.maximum |> (+1)
  let grid = check_all_points points (i64.i32 max_x) (i64.i32 max_y)
  let edge = grid[0] ++ grid[max_x-1] ++ grid[:,0] ++ grid[:,max_y-1]
  let on_edge i = any (==i) edge
  let area_per_point = flatten grid |> histogram num_points
  in map2 (\i v -> if on_edge i then 0 else v)
          (map i32.i64 (iota num_points))
          area_per_point
     |> i32.maximum

entry part2 [num_points] (input: [num_points][2]i32) =
  let points = parse input
  let max_x = points |> map (.x) |> i32.maximum |> (+1)
  let max_y = points |> map (.y) |> i32.maximum |> (+1)
  let f x y = map (distance {x=i32.i64 x,y=i32.i64 y}) points |> i32.sum
  let limit = 10000
  in tabulate_2d (i64.i32 max_x) (i64.i32 max_y) f |> flatten |> count (<limit)
