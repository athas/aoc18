-- Massively parallel brute force with a cellular automaton covering
-- the _entire_ space for each iteration.  This is hilariously
-- inefficient, so good thing I have a 10TFLOPS GPU!

type cell =
  #clay | #sand | #water | #water_rest | #water_resting_from_west | #water_resting_from_east

let cell_to_ascii (c: cell) = match c case #clay -> '#'
                                      case #sand -> '.'
                                      case #water -> '|'
                                      case #water_rest -> '~'
                                      case #water_resting_from_west -> '/'
                                      case #water_resting_from_east -> '\\'

let water (c: cell) =
  c == #water ||
  c == #water_resting_from_west ||
  c == #water_resting_from_east ||
  c == #water_rest

let resting (c: cell) =
  c == #water_rest ||
  c == #water_resting_from_west ||
  c == #water_resting_from_east ||
 c == #clay

let firm (c: cell) =
  c == #water_rest || c == #clay

let parse_world input: (i32, *[][]cell) =
  let min_x = input |> map (.[0]) |> i32.minimum
  let min_y = input |> map (.[1]) |> i32.minimum
  let max_x = input |> map (.[0]) |> i32.maximum |> (+1) -- ???
  let max_y = input |> map (.[1]) |> i32.maximum
  let w = max_x - min_x + 1
  let h = max_y - min_y + 1
  let blank_world: *[]cell = replicate (h*w) #sand
  let clays = map (\p -> (p[1]-min_y)*w+(p[0]-min_x)) input
  let world = scatter blank_world clays (map (const #clay) clays)
              |> unflatten h w
  in (500-min_x, copy world)

let step [h][w] water_x (world: [h][w]cell): [h][w]cell =
  let get y x: cell = if y >= 0 && x >= 0 && y < h && x < w
                      then unsafe world[y,x] else #sand
  let step' y x =
    let (n,w,e,s,sw,se) = (get (y-1) x, get y (x-1), get y (x+1), get (y+1) x,
                           get (y+1) (x-1), get (y+1) (x+1))
    let c = get y x
    in if firm c then c else
    let c = if w == #water_rest || e == #water_rest
            then #water_rest else c
    let c = if c == #water && firm s && firm w
            then #water_resting_from_west else c
    let c = if c == #water && firm s && firm e
            then #water_resting_from_east else c
    let c = if c == #water && w == #water_resting_from_west
            then #water_resting_from_west else c
    let c = if c == #water && e == #water_resting_from_east
            then #water_resting_from_east else c
    let c = if c == #water_resting_from_west &&
               (e == #water_resting_from_east || e == #water_rest || e == #clay)
            then #water_rest else c
    let c = if c == #water_resting_from_east &&
               (w == #water_resting_from_west || w == #water_rest || w == #clay)
            then #water_rest else c
    let c = if !(water c) && (water n || ((water e && firm se || water w && firm sw)))
            then #water else c
    let c = if y == 0 && x == water_x
            then #water else c
    in c
  in tabulate_2d h w step'

let step_until_fixed_point water_x (world: [][]cell): [][]cell =
  (loop (world, continue) = (world, true) while continue do
   let world' = step water_x world
   in (world', world != world')).0

entry part1 (input: [][2]i32) =
  let (water_x, world) = parse_world input
  --  let world = iterate steps (step water_x) world
  let world = step_until_fixed_point water_x world
  let waters = flatten world |> map water |> map i32.bool |> i32.sum
  in waters -- map (map cell_to_ascii) world

entry part2 (input: [][2]i32) =
  let (water_x, world) = parse_world input
  --  let world = iterate steps (step water_x) world
  let world = step_until_fixed_point water_x world
  let waters = flatten world |> map (==#water_rest) |> map i32.bool |> i32.sum
  in waters -- map (map cell_to_ascii) world
