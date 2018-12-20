-- Implementing an NFA with dynamic state deduplication in Futhark is
-- the fucking worst.

let dedup 't [n] (eq: t -> t -> bool) (xs: [n]t): (*[n]t, i32) =
  loop (new, i) = (replicate n xs[0], 0) for x in xs do
  if any (eq x) new[:i]
  then (new, i)
  else (new with [i] = x, i + 1)

type pos = (i32, i32)

type dir = i32

let delta (d: dir): (i32, i32) =
  match d case 'W' -> (0, -1)
          case 'E' -> (0, 1)
          case 'N' -> (-1, 0)
          case 'S' -> (1, 0)
          case _ -> assert false (0,0)

let walk [k] (world: *[k][k]i32) ((x,y): pos) (d: dir): (*[k][k]i32, pos) =
  let (dx, dy) = delta d
  in (world
      with [x+dx,y+dy] = '.'
      with [x+dx+dx,y+dy+dy] = '.',
      (x+dx+dx, y+dy+dy))

let add_states [n] (to_states: *[n]pos, to_num_states: i32)
                   (from_states: [n]pos, from_num_states: i32)
               : (*[n]pos, i32) =
  loop (to_states, to_num_states) for i < from_num_states do
  let (to_states, to_num_states) = if to_num_states == n then dedup (==) to_states
                                   else (to_states, to_num_states)
  let to_states[to_num_states] = from_states[i]
  in (to_states, to_num_states+1)

let step [n] (world: *[][]i32)
             (stack: *[]([n]pos,i32,i32), stack_top: i32)
             (c: i32)
           : (*[][]i32, (*[]([n]pos,i32,i32), i32))=
  let (states, num_states, to_pop) = (stack[stack_top-1]) in
  match c
  case '(' ->
         (world,
          (stack with [stack_top] = (copy states, num_states, 1), stack_top+1))
         case ')' ->
    let (states, num_states, new_to_pop) = copy (stack[stack_top-to_pop-1]) in
    let (states, num_states) =
      loop (states, num_states)
      for i in stack_top-to_pop..<stack_top do
      add_states (states, num_states) (stack[i].1, stack[i].2)
    let stack[stack_top-to_pop-1] = (states, num_states, new_to_pop)
    in (world, (stack, stack_top-to_pop))
  case '|' ->
    let (states, num_states, _) = (stack[stack_top-to_pop-1]) in
    (world,
     (stack with [stack_top] = (copy states, num_states, to_pop+1), stack_top+1))
  case _ ->
    let (world, states) =
      loop (world, states) = (world, copy states) for i < num_states do
      let (world, state) = walk world states[i] c
      in (world, states with [i] = state)
    let stack[stack_top-1] = (states, num_states, to_pop)
    in (world, (stack, stack_top))

let build_maze (input: []i32) =
  let input = input |> tail |> init
  let k = 500 -- half of world edge size.
  let n = 2000 -- max number of NFA states.
  let world = replicate k (replicate k '#')
  let pos = (k/2, k/2)
  let world[pos.1, pos.2] = '.'
  let states = (replicate n pos, 1, 1)
  let stack = (replicate (length input) states, 1)
  let (world, _) = loop (world, stack) for c in input do step world stack c
  in (world, pos)

let room_distances (input: []i32) =
  let (maze, pos) = build_maze input
  let k = length maze

  -- OK, we have the maze; now to find the most distant room.  I will
  -- do this with a stencil, because I do everything with a stencil.
  -- This is not my best trick.
  let wall = i32.highest/2
  let not_reached = wall-1
  let grid = map (map (\c -> if c == '#' then wall else not_reached)) maze
  let grid[pos.1, pos.2] = 0
  let continue = true
  let (grid, _) =
    loop (grid, continue) while continue do
    let get x y =
      if x >= 0 && y >= 0 && x < k && y < k
      then grid[x,y] else i32.highest
    let evolve x y =
      let (n, s, e, w, c) =
        (get (x-1) y, get (x+1) y,
         get x (y+1), get x (y-1),
         get x y)
      in if c == wall then c
         else (n+1) `i32.min` (s+1) `i32.min` (e+1) `i32.min` (w+1) `i32.min` c
    let new_grid = tabulate_2d k k evolve
    in (new_grid, new_grid != grid)

  in grid |> flatten |> map (\x -> if x==wall || x==not_reached then 0 else x/2)

entry part1 (input: []i32) =
  room_distances input |> i32.maximum

entry part2 (input: []i32) =
  room_distances input |> map (>=1000) |> map i32.bool |> i32.sum |> (/2) |> (+1)
