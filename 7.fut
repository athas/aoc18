-- This one is Death.

let topo_next [num_nodes] (removed: [num_nodes]bool) (inserted: [num_nodes]bool) edges =
  let satisfied inserted i (from, to) =
    unsafe to != i || inserted[from]
  let insertable inserted i =
    if unsafe !removed[i] && all (satisfied inserted i) edges
    then i else num_nodes
  in map (insertable inserted) (iota num_nodes) |> i32.minimum

let toposort (edges: [](i32, i32)): []i32 =
  -- num_nodes relies on a gross assumpption, but it's fast!
  let num_nodes = i32.maximum (map (.1) edges) - i32.minimum (map (.0) edges) + 1
  let (_, order) =
    (loop (inserted, order) = (replicate num_nodes false, replicate num_nodes 0)
     for i < num_nodes do
     let next = topo_next inserted inserted edges
     in (inserted with [next] = true, order with [i] = next))
  in order

entry part1 (input: [][]i32) =
  let edges = map (\r -> (r[0], r[1])) input
  in toposort edges

type worker = {i:i32, t:i32}
let available: worker = {i= -1, t = 0}
let busy = (!=available)

let num_workers: i32 = 5

let any_busy: []worker -> bool = foldl (\x w -> x || busy w) false

let find_available (ws: [num_workers]worker) =
  loop i = 0 while i < num_workers && busy ws[i] do i + 1

let next_end (ws: [num_workers]worker) =
  map (\w -> if busy w then w.t else i32.highest) ws |> i32.minimum

entry part2 (input: [][]i32) =
  let edges = map (\r -> (r[0], r[1])) input
  let num_nodes = i32.maximum (map (.1) edges) - i32.minimum (map (.0) edges) + 1
  let duration i = i + 61
  let (_, seconds, _, _, _) =
    loop (workers, now, num_done, removed, done) =
         (replicate num_workers available, 0, 0, replicate num_nodes false, replicate num_nodes false)
     while num_done < num_nodes || any_busy workers do
     let (done, workers, num_done) = loop (done, workers, num_done) for i < num_workers do
       let w = workers[i] in
       if busy w && now >= w.t
       then (done with [w.i] = true, workers with [i] = available, num_done + 1)
       else (done, workers, num_done)
     let next = topo_next removed done edges
     let wi = find_available workers
     in if next < num_nodes
        then (if wi < num_workers
              then let removed[next] = true
                   in (workers with [wi] = {t=now+duration next, i=next}, now, num_done, removed, done)
              else (workers, next_end workers, num_done, removed, done))
        else let thence = if ! (any_busy workers) then now
                          else next_end workers
             in (workers, thence, num_done, removed, done)
  in seconds
