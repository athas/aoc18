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

let move (n: i32) (p: particle) = p with pos = {x=p.pos.x + p.vel.dx * n, y=p.pos.y + p.vel.dy * n}

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

entry solve (input: [][4]i32) =
  let particles = map parse input
  let n = min_to_converge particles
  let particles = map (move n) particles
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
  let raster p = (p.pos.y - min_y) * w + (p.pos.x - min_x)
  let image = scatter blank_canvas (map raster particles) (replicate (h*w) 1u8)
  in (n, unflatten h w image)

entry part1 = solve >-> (.2)

entry part2 = solve >-> (.1)
