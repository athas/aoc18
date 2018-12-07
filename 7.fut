-- This one is Death.

let topo_next [num_nodes] (inserted: [num_nodes]bool) edges =
  let satisfied inserted i (from, to) =
    unsafe to != i || inserted[from]
  let insertable inserted i =
    if unsafe !inserted[i] && all (satisfied inserted i) edges
    then i else num_nodes
  in map (insertable inserted) (iota num_nodes) |> i32.minimum

let toposort (edges: [](i32, i32)): []i32 =
  -- num_nodes relies on a gross assumpption, but it's fast!
  let num_nodes = i32.maximum (map (.2) edges) - i32.minimum (map (.1) edges) + 1
  let (_, order) =
    (loop (inserted, order) = (replicate num_nodes false, replicate num_nodes 0)
     for i < num_nodes do
     let next = topo_next inserted edges
     in (inserted with [next] = true, order with [i] = next))
  in order

entry part1 (input: [][]i32) =
  let edges = map (\r -> (r[0], r[1])) input
  in toposort edges
