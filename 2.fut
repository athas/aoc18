-- This crucially depends on all box IDs having the same length, as
-- otherwise we end up with irregular arrays, and working with those
-- in Futhark is a good way to lose the Christmas spirit.

type box_id = []i8
type signature = {double: bool, triple: bool}

let occurrences_in (x: i8) (ys: []i8): i32 =
  map (==x) ys |> map (i32.bool) |> i32.sum

-- Use a naive O(n**2) implementation here.  This can also be done
-- with a segmented scan, but that is massively overkill for small box
-- IDs.
let signature (x: box_id): signature =
  let occurs = map (`occurrences_in` x) x
  in {double = any (==2) occurs, triple = any (==3) occurs}

let main (ids: []box_id) =
  ids
  |> map signature
  |> map (\{double, triple} -> (i32.bool double, i32.bool triple))
  |> reduce_comm (\(a,b) (c,d) -> (a+c, b+d)) (0, 0)
  |> uncurry (*)
