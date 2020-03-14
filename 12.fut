-- This problem is nice and parallel and permits an efficient
-- implementation, *but* the input is far too small for it to be
-- worthwhile in practice.  Still, we must scale!

-- The array of patterns is indexed by the neighbourhood of a cell
-- viewed as a binary number (with plant=1).
type~ patterns = []bool

let pattern (pat: [5]i32): i32 =
  let bit x i = (x&1) << i
  in bit pat[0] 4 | bit pat[1] 3 | bit pat[2] 2 | bit pat[3] 1 | bit pat[4] 0

let parse_patterns (patterns_raw: [][6]i32): patterns =
  let parse_pattern line = (let pat = line[:5]
                            let to = line[5]
                            in (pattern pat, bool.i32 to))
  let scratch = replicate 32 false
  let (patterns, to) = unzip (map parse_pattern patterns_raw)
  in scatter scratch patterns to

let evolve [n] (patterns: patterns) (state: [n]bool) =
  let get i = if i < 0 || i >= length state then 0 else unsafe i32.bool state[i]
  let new_cell i = unsafe patterns[pattern [get (i-2), get (i-1), get i, get (i+1), get (i+2)]]
  in tabulate n new_cell

let sum_live_pots [n] (shift: i64) (state: [n]bool) =
  map2 (*) (map i64.bool state) (map (+shift) (iota n |> map i64.i32))
  |> i64.sum

entry part1 (initial_raw: []i32) (patterns_raw: [][6]i32) =
  let patterns = parse_patterns patterns_raw
  let generations = 20
  let initial = replicate generations false ++
                map bool.i32 initial_raw ++
                replicate generations false
  let final_state = loop s = initial for _i < generations do evolve patterns s
  in sum_live_pots (i64.i32 (-generations)) final_state

type~ state = {shift: i32, cells: []bool}

let state_match [n] (x: [n]bool) (y: [n]bool) =
  let yget r i = let j = r+i
                 in if j < 0 || j >= n then false else unsafe y[j]
  in map3 (\b x y -> if b then x else y)
          (tabulate (n-1) (\r -> map2 (==) x (map (yget (r+1)) (iota n)) |> and))
          (map (+1) (iota (n-1)))
          (replicate (n-1) i32.highest)
      |> i32.minimum

-- WHACK ASSUMPTION: a cycle can at most only move by one cell.
entry part2 (initial_raw: []i32) (patterns_raw: [][6]i32) =
  let patterns = parse_patterns patterns_raw
  let search_region = 200 -- heuristic
  let basic_shift = 2
  let initial = replicate basic_shift false ++
                map bool.i32 initial_raw ++
                replicate search_region false
  let (state, first_repeat, _) =
    (loop (state, i, repeat_found) = (initial, 1, false)
     while !repeat_found do
     let new_state = evolve patterns state
     in if and (map2 (==) state (rotate 1 new_state))
        then (state, i-1, true)
        else (new_state, i+1, false))
  -- Now we know that state 'first_repeat' is the start of a repeating
  -- cycle, shifted right by 1.
  let generations = 50000000000i64
  let shift = generations - i64.i32 first_repeat - i64.i32 basic_shift
  in (first_repeat, sum_live_pots shift state)
