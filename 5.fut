-- Brute force, but elegant!

type unit = i8

let polarisation (x: unit) = (x >> 5) & 1
let kind (x: unit) = x % 32

let annihilate [n] (dir: i32) (units: [n]i8) =
  let keep i me =
    let j = i + (if i%2==0 then dir else -dir)
    in if j >= 0 && j < n then let other = unsafe units[j]
                               in kind me != kind other ||
                                  polarisation me == polarisation other
                          else true
  in map (.1) (filter (.0) (zip (map2 keep (iota n) units) units))

let annihilate_fixed_point (units: []i8) =
  (loop (units, continue) = (units, true) while continue do
   let orig_len = length units
   let units' = units |> annihilate (-1) |> annihilate 1
   in (units', length units' != orig_len))
  |> (.0)

entry part1 (units: []i8) =
  units |> annihilate_fixed_point |> length

let num_kinds = 'Z' - 'A' + 1
let index_kinds i = i8.i32 ('A' + i)

entry part2 (units: []i8) =
  -- Shorten the array as much as possible immediately.
  let units = annihilate_fixed_point units
  let not_kind x y = kind x != kind y
  let check_without_kind i = part1 (filter (not_kind (index_kinds i)) units)
  let f cur_best i = i32.min cur_best (check_without_kind i)
  in foldl f (length units) (iota num_kinds)
