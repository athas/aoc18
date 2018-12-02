let occurences_in (x: i32) (ys: []i32): i32 =
  map (==x) ys |> map (i32.bool) |> i32.sum

let member (x: i32) (ys: []i32): bool =
  any (==x) ys

let main (xs: []i32) =
  let (first_repeat, _, _) =
    (loop (cur, seen, continue) = (0, [0], true) while continue do
     let new_freqs = scan (+) 0 xs |> map (+cur)
     -- Check if any of these frequencies have been seen before.
     let seen_before = filter (`member` seen) new_freqs
     -- Then if any occur duplicate here.
     let dup_here = filter ((`occurences_in` new_freqs) >-> (>1)) new_freqs
     in if length seen_before > 0 then (seen_before[0], seen, false)
        else if length dup_here > 0 then (dup_here[0], seen, false)
        else (last new_freqs, seen ++ new_freqs, true))
  in first_repeat
