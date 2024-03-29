-- This crucially depends on all box IDs having the same length, as
-- otherwise we end up with irregular arrays, and working with those
-- in Futhark is a good way to lose the Christmas spirit.

type box_id [n] = [n]i8
type signature = {double: bool, triple: bool}

let occurrences_in (x: i8) (ys: []i8): i64 =
  map (==x) ys |> map (i64.bool) |> i64.sum

-- Use a naive O(n**2) implementation here.  This can also be done
-- with a segmented scan, but that is massively overkill for small box
-- IDs.
let signature (x: box_id []): signature =
  let occurs = map (`occurrences_in` x) x
  in {double = any (==2) occurs, triple = any (==3) occurs}

entry part1 (ids: [](box_id [])) =
  ids
  |> map signature
  |> map (\{double, triple} -> (i64.bool double, i64.bool triple))
  |> reduce_comm (\(a,b) (c,d) -> (a+c, b+d)) (0, 0)
  |> uncurry (*)

let matching (x: box_id []) (y: box_id []): bool =
  map2 (!=) x y |> map i64.bool |> i64.sum |> (==1)

let find_match [n] (x: box_id []) (ids: [n]box_id []): i64 =
  map2 (\i y -> if matching x y then i else (-1)) (iota n) ids
  |> i64.maximum

-- Since there are so few boxes in the input (250), let's just do a
-- check of the Cartesian product.  This is twice the amount of work
-- (and perhaps much more if we could find an early match), but has a
-- simpler memory access pattern.
entry part2 (ids: [](box_id [])) =
  let matches = map (`find_match` ids) ids |> filter (>=0)
  let x = ids[matches[0]]
  let y = ids[matches[1]]
  in map (.0) (filter (.2) (zip3 x y (map2 (==) x y)))
