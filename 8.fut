-- I'm only doing the first one here, because tree processing is
-- pointlessly painful in Futhark.  This is not what I signed up for!

entry part1 (xs: []i32) =
  let stack = replicate (length xs) (0,0)
  let stack[0] = (xs[0], xs[1])
  let (_, _, _, meta_sum) =
    (loop (stack, i, j, meta_sum) = (stack, 0, 2, 0)
     while i >= 0 do
     let (children, meta) = stack[i]
     in if children > 0 then let stack[i] = stack[i] with 1 = children - 1
                             let stack[i+1] = (xs[j], xs[j+1])
                             in (stack, i+1, j+2, meta_sum)
        else (stack, i-1, j+meta, meta_sum + foldl (+) 0 xs[j:j+meta]))
  in meta_sum
