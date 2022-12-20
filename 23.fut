-- Got lucky here, I think.

type pos = {x: i32, y: i32, z: i32}
let origo: pos = {x=0,y=0,z=0}

let dist (p1: pos) (p2: pos) =
  i32.abs (p1.x-p2.x) + i32.abs (p1.y-p2.y) + i32.abs (p1.z-p2.z)

type bot = {p: pos, r: i32}

let nilbot: bot = {p=origo, r=0}

let in_range_of (p1: bot) (p2: bot) =
  dist p1.p p2.p <= p2.r

let maximum_by 't (f: t -> i32) (x: t) (ts: []t): t =
  reduce (\a b -> if f a < f b then b else a) x ts

let to_bot l = {p={x=l[0], y=l[1], z=l[2]}, r=l[3]}

entry part1 (input: [][]i32) =
  let bots = map to_bot input
  let largest = maximum_by (.r) nilbot bots
  let n = map (`in_range_of` largest) bots |> map i32.bool |> i32.sum
  in n

import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/sorts/radix_sort"

let nub 't (eq: t -> t -> bool) (ts: []t): []t =
  map (.1) (filter (.0) (zip (map not (map2 eq ts (rotate 1 ts))) ts))

let zip' [n] [m] 'x 'y (xs: [n]x) (ys: [m]y) : [n](x,y) =
  zip xs (ys :> [n]y)

entry part2 (input: [][]i32) =
  let bots = map to_bot input
  let distf {p,r} = let d = p.x + p.y + p.z
                    in [(d-r, 1), (d+r+1, -1)]
  let dist = map distf bots
                 |> flatten
                 |> radix_sort_int_by_key (.0) i32.num_bits i32.get_bit
  let flags = map2 (!=) (map (.0) dist) (rotate 1 (map (.0) dist))
  let dist = zip' (nub (==) (map (.0) dist))
                  (segmented_reduce (+) 0i32 flags (map (.1) dist))
  let run = zip' (map (.0) dist)
                 (scan (+) 0 (map (.1) dist))
  let max = i32.maximum (map (.1) run)
  let _ = trace run
  let intervals = zip' (init run) (tail run)
                      |> filter ((.0) >-> (.1) >-> (==max))
                      |> map (\((a,_),(b,_)) -> (a, b-1))
  in if any (\(a,b) -> a <= 0 && b >= 0) intervals
     then 0
     else map (\(a,b) -> if b < 0 then -b else a) intervals |> i32.minimum
