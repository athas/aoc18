-- This solution is pretty simple and effective, but it's your own job
-- to decode the resulting image into text.
--
-- My convergence criteria is to stop as soon as all particles have a
-- neighbour.  This means that I can once and for all compute how many
-- seconds it will take, and then move them all in one fell swoop!

type pos = {x:i32, y:i32}
type vel = {dx:i32, dy:i32}
type particle = {pos: pos, vel: vel}

let parse (line: [4]i32) = {pos={x=line[0],y=line[1]}, vel={dx=line[2],dy=line[3]}}

let distance_x (p1: particle) (p2: particle) =
  i32.abs (p1.pos.x - p2.pos.x)

let distance_y (p1: particle) (p2: particle) =
  i32.abs (p1.pos.y - p2.pos.y)

let distance (p1: particle) (p2: particle) =
  distance_x p1 p2 + distance_y p1 p2

let neighbours (p1: particle) (p2: particle) =
  distance_x p1 p2 <= 1 && distance_y p1 p2 <= 1 && p1 != p2

let move (n: i32) (p: particle) =
  p with pos = {x=p.pos.x + p.vel.dx * n, y=p.pos.y + p.vel.dy * n}

let simulate (ps: []particle) (t: i32) =
  map (move t) ps

let converged (ps: []particle) =
  all (\p -> any (neighbours p) ps) ps

let min_to_converge (ps: []particle): i32 =
  let f distf =
    (let min_to p1 p2 =
       let d1 = distf p1 p2
       let d2 = distf (move 1 p1) (move 1 p2)
       in if p1 == p2 || d2 >= d1 then i32.highest else i32.abs d1/i32.abs (d2-d1)
       in ps |> map (\p -> map (min_to p) ps |> i32.minimum) |> i32.maximum)
  in i32.max (f distance_x) (f distance_y)

let solve (input: [][4]i32) =
  let particles = map parse input
  let n = min_to_converge particles
  let particles = simulate particles n
  let min_x = particles |> map (.pos.x) |> i32.minimum
  let min_y = particles |> map (.pos.y) |> i32.minimum
  let max_x = particles |> map (.pos.x) |> i32.maximum
  let max_y = particles |> map (.pos.y) |> i32.maximum
  -- Create a smaller image containing just the interesting area.  In
  -- principle, this might still be gigantic, but in practice it is
  -- probably not.
  let w = max_x - min_x + 1
  let h = max_y - min_y + 1
  let blank_canvas = replicate (h*w) 0u8
  let raster (p: particle) = (p.pos.y - min_y) * w + (p.pos.x - min_x)
  let image = scatter blank_canvas (map raster particles) (map (const 1) particles)
  in (n, unflatten h w image)

entry part1 input = input |> solve |> (.1)

entry part2 input = input |> solve |> (.0)

let box (ps: []particle) =
  let minx = ps |> map (.pos.x) |> i32.minimum
  let maxx = ps |> map (.pos.x) |> i32.maximum
  let miny = ps |> map (.pos.y) |> i32.minimum
  let maxy = ps |> map (.pos.y) |> i32.maximum
  in (minx, miny, maxx-minx, maxy-miny)

entry mebeim (input: [][4]i32) =
  let particles = map parse input
  let (t, _) =
    loop (t1, t2) = (0, 100000) while t1 != t2 do
    let t = (t1 + t2) / 2
    let sims = map (\d -> (box(simulate particles (t + d))).3) [-1, 0, 1]
    let l = sims[0]
    let m = sims[1]
    let r = sims[2]
    in trace (if l > m && m < r then (t, t)
       else if l < m then (t1, t)
       else (t, t2))
  in t
