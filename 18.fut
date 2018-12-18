-- This one works quite well.  Part 1 is a straightforward 2D cellular
-- automaton, and part 2 is that plus cycle detection.

type cell = #open | #trees | #lumberyard

let cell_from_ascii (c: i32): cell =
  if c == '.' then #open
  else if c == '|' then #trees
  else #lumberyard

let cell_to_ascii (c: cell) =
  match c
  case #open -> '.'
  case #trees -> '|'
  case #lumberyard -> '#'

type neighbourhood = {nw: cell, n: cell, ne: cell,
                      w: cell, c: cell, e: cell,
                      sw: cell, s: cell, se: cell}

let neighbourhood [n][m] (x: i32) (y: i32) (cells: [n][m]cell): neighbourhood =
  let get x' y' = if x' >= 0 && x' < n && y' >= 0 && y' < m
                  then unsafe cells[x', y'] else #open
  in {nw=get (x-1) (y-1), n=get (x-1) y, ne = get (x-1) (y+1),
      w=get x (y-1), c=get x y, e = get x (y+1),
      sw=get (x+1) (y-1), s=get (x+1) y, se = get (x+1) (y+1)}

let count what (area: neighbourhood): i32 =
  let p x = i32.bool (x == what)
  in p area.nw + p area.n + p area.ne +
     p area.w + p area.e +
     p area.sw + p area.s + p area.se

let step [n][m] (cells: [n][m]cell): *[n][m]cell =
  let evolve x y =
    let area = neighbourhood x y cells
    in match area.c
       case #open -> if count #trees area >= 3
                      then #trees else #open
       case #trees -> if count #lumberyard area >= 3
                      then #lumberyard else #trees
       case #lumberyard -> if count #lumberyard area > 0 &&
                              count #trees area > 0
                           then #lumberyard else #open
  in tabulate_2d n m evolve

entry part1 (input: [][]i32) =
  let cells = map (map cell_from_ascii) input
  let steps = 10
  let cells = iterate steps step cells
  let num_lumberyards = cells |> flatten |> map (==#lumberyard) |> map i32.bool |> i32.sum
  let num_trees = cells |> flatten |> map (==#trees) |> map i32.bool |> i32.sum
  in num_lumberyards * num_trees

let find_repeat [h] (history: [h][][]cell) =
  let truths = map (\x -> map (==x) history) history
  in tabulate_2d h h (\i j -> ((i,j), i!=j && truths[i,j])) |> flatten |> filter (.2) |> map (.1)

-- Bloody loop detection again!
entry part2 (input: [][]i32) =
  let cells = map (map cell_from_ascii) input
  let steps = 1000 -- hopefully enough
  let history = replicate steps cells
  let history =
    loop history for i in 1..<steps do
      history with [i] = step history[i-1]
  let (repeater, repeats_at) = (find_repeat history)[0]
  let period = repeats_at - repeater
  let desired_steps = 1000000000
  let steps_to_go = (desired_steps - repeats_at) % period
  let cells = iterate steps_to_go step history[repeats_at]
  let num_lumberyards = cells |> flatten |> map (==#lumberyard) |> map i32.bool |> i32.sum
  let num_trees = cells |> flatten |> map (==#trees) |> map i32.bool |> i32.sum
  in num_lumberyards * num_trees
