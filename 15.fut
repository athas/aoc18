-- This one is real bad.  The solution is slow, mainly because of a
-- very naive tracking of positions (and overkill in the pathfinding).
-- There is no parallelism to speak of (except in the pathfinding,
-- where it is overdone).  This was not a good showing.  I am tired.

-- Bit vector indicating the state of the cell.  Bits denote:
--
-- 0: set if wall
-- 1: set if elf
-- 2: set if goblin
--
-- 15-31: health if goblin or elf.
type cell = u32

type inhabitant = #elf | #goblin | #wall | #floor
type creature = #elf | #goblin

let cell_wall (c: cell) = c & 1 == 1
let cell_elf (c: cell) = c & 0b10 == 0b10
let cell_goblin (c: cell) = c & 0b100 == 0b100
let cell_inhabitant (c: cell): inhabitant = match c & 0b111
                                            case 1 -> #wall
                                            case 2 -> #elf
                                            case 4 -> #goblin
                                            case _ -> #floor
let cell_health (c: cell): i32 = i32.u32 (c >> 16)
let cell_set_health (c: cell) (x: i32): cell = (c & 0xFF) | u32.i32 x << 16
let cell_dec_health (c: cell) (x: i32): cell = cell_set_health c (cell_health c - x)

let cell_to_ascii c = if cell_wall c then '#'
                      else if cell_elf c then 'E'
                      else if cell_goblin c then 'G'
                      else '.'

let cell_from_ascii c: cell = if c == '#' then 1
                              else if c == 'E' then cell_set_health 0b10 200
                              else if c == 'G' then cell_set_health 0b100 200
                              else 0

let no_path = i32.highest-1

let neighbourhood x y arr =
  let n = arr[x-1,y]
  let s = arr[x+1,y]
  let w = arr[x,y-1]
  let e = arr[x,y+1]
  in {n, s, w, e}

-- This is the interesting function.  It does multi-sink shortest
-- path.  Pretty much the only reason I solved this is to write this
-- function, which is cool and good.  It is inefficient overkill,
-- though.
let shortest_paths [h][w] (cells: [h][w]cell) (source: inhabitant) =
  let dists = replicate w no_path |> replicate h
  -- The update function assumes that the borders are always boring.
  let update dists x y =
    (if x == 0 || y == 0 || x == h-1 || y == w-1 then (dists[x,y], false)
     else let {n,s,w,e} = neighbourhood x y dists
          let now = dists[x,y]
          let next = if cell_inhabitant cells[x,y] == source then 0
                     else if cell_inhabitant cells[x,y] == #floor
                     then now `i32.min` n + 1 `i32.min` s + 1 `i32.min` w + 1 `i32.min` e + 1
                     else no_path
          in (next, now != next))
  in (loop (dists, continue) = (dists, true) while continue do
        let (dists', changes) = tabulate_2d h w (update dists) |> map unzip |> unzip
        in (dists', flatten changes |> or)).1

let creature_movement x y (cells: [][]cell) enemy =
  let {n,s,w,e} = shortest_paths cells enemy |> neighbourhood x y
  let adjacent = n * s * w * e == 0
  let go_n = n != no_path && n <= w && n <= e && n <= s
  let go_w = w != no_path && w <= e && w <= s
  let go_e = e != no_path && e <= s
  let go_s = s != no_path
  in if adjacent then (x,y)
     else if go_n then (x-1, y)
     else if go_w then (x, y-1)
     else if go_e then (x, y+1)
     else if go_s then (x+1, y)
     else (x,y)

let creature_find_target [h][w] (x: i32) (y: i32) (cells: [h][w]cell) (enemy: inhabitant): (i32, i32) =
  let {n,s,w,e} = neighbourhood x y cells
  let health c = if cell_inhabitant c == enemy then cell_health c else i32.highest
  let (n_h, s_h, w_h, e_h) = (health n, health s, health w, health e)
  let attack_n = cell_inhabitant n == enemy && n_h <= w_h && n_h <= e_h && n_h <= s_h
  let attack_w = cell_inhabitant w == enemy && w_h <= e_h && w_h <= s_h
  let attack_e = cell_inhabitant e == enemy && e_h <= s_h
  let attack_s = cell_inhabitant s == enemy
  in      if attack_n then (x-1,y)
     else if attack_w then (x,y-1)
     else if attack_e then (x,y+1)
     else if attack_s then (x+1,y)
     else (-1,-1)

let creature_attack damage x' y' (cells: *[][]cell): *[][]cell =
  let enemy = cells[x',y']
  let health = cell_health enemy
  in if damage >= health
     then cells with [x',y'] = cell_from_ascii '.'
     else cells with [x',y'] = cell_dec_health enemy damage

let creature_act [h][w] damage (x: i32) (y: i32)
                        (cells: *[h][w]cell) (enemy: inhabitant): *[h][w]cell =
  let (x', y') = creature_movement x y cells enemy
  let c = cells[x,y]
  let cells[x,y] = cell_from_ascii '.'
  let cells[x',y'] = c
  let (x'', y'') = creature_find_target x' y' cells enemy
  in if (x'', y'') == (-1,-1)
     then cells
     else creature_attack damage x'' y'' cells

let act [h][w] elf_attack (x: i32) (y: i32) (cells: *[h][w]cell): *[h][w]cell =
  match cell_inhabitant cells[x,y]
  case #wall -> cells
  case #floor -> cells
  case c -> let enemy = if c == #elf then #goblin else #elf
            let damage = if c == #elf then elf_attack else 3
            in creature_act damage x y cells enemy

let find_inhabitants [h][w] (cells: [h][w]cell): *[](i32,i32) =
  let is_creature c = c == #elf || c == #goblin in
  copy (tabulate_2d h w (\x y -> ((x,y), cell_inhabitant cells[x,y]))
        |> flatten
        |> filter ((.2) >-> is_creature)
        |> map (.1))

let everybody_act [h][w] elf_damage inhabitants (cells: *[h][w]cell): *[h][w]cell =
  loop cells for (x,y) in inhabitants do act elf_damage x y cells

let victory (inhabitants: []inhabitant) =
  all (== #goblin) inhabitants || all (== #elf) inhabitants

let run [h][w] elf_damage (cells: *[h][w]cell) =
  loop (cells, inhabitants, steps) = (cells, find_inhabitants cells, 0i32)
  while !(victory (map (\(x,y) -> cell_inhabitant cells[x,y]) inhabitants)) do
  let cells' = everybody_act elf_damage inhabitants cells
  in (cells', find_inhabitants cells', steps + 1)

entry part1 (input: [][]i32) =
  let cells = map (map cell_from_ascii) input
  let (cells, inhabitants, steps) = run 3 cells
  let _ascii = map (map cell_to_ascii) cells
  in inhabitants |> map (\(x,y) -> cell_health cells[x,y]) |> i32.sum |> (*(steps-1))

let count_elves cells = cells |> flatten |> filter (cell_inhabitant >-> (== #elf)) |> length

entry part2 (input: [][]i32) =
  let cells = map (map cell_from_ascii) input
  let num_elves = count_elves cells
  let (score, _, _) =
    loop (_, continue, power) = (0, true, 3) while continue do
      let (cells, inhabitants, steps) = run power (copy cells)
      in if count_elves cells == num_elves
         then let _ = trace (inhabitants |> map (\(x,y) -> cell_health cells[x,y]))
              in (inhabitants |> map (\(x,y) -> cell_health cells[x,y]) |> i32.sum |> (*(steps-1)),
                  false, power)
         else (0, true, power + 1)
  in score
