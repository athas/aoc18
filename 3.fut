-- For this one I got really lazy.  Very brute force.  Would scale
-- very poorly for a sparse grid.

type area = { id: i32, left: i32, top: i32, width: i32, height: i32 }

let in_area (x: i32) (y: i32) (area: area): bool =
  x >= area.left && x < area.left + area.width &&
  y >= area.top && y < area.top + area.height

let area_span_x (area: area) =
  area.left + area.width

let area_span_y (area: area) =
  area.top + area.height

let to_area (raw: []i32): area =
  {id=raw[0], left=raw[1], top=raw[2], width=raw[3], height=raw[4]}

let area_num_pixels (area: area) =
  area.width * area.height

let area_get_pixel (area: area) i =
  (i / area.width, i % area.width)

let area_get_pixel_flat max_x area i =
  let (x, y) = area_get_pixel area i
  in (x + area.top + (y + area.left) * max_x)

import "lib/github.com/diku-dk/segmented/segmented"

let grid_claims areas: [][]i32 =
  let max_x = map area_span_x areas |> i32.maximum
  let max_y = map area_span_y areas |> i32.maximum
  let points = areas |> expand area_num_pixels (area_get_pixel_flat max_x)
  in reduce_by_index (replicate (max_x*max_y) 0) (+) 0 points (replicate (max_x*max_y) 1i32)
     |> unflatten max_x max_y

entry part1 (input: [][]i32) =
  let areas = map to_area input
  let occupied = grid_claims areas |> flatten
  in occupied |> map (>1) |> map i32.bool |> i32.sum

let has_sole_claim (grid: [][]i32) (area: area): bool =
  loop ok = true for x in area.left..<area.left+area.width do
  loop ok for y in area.top..<area.top+area.height do
  unsafe ok && grid[x,y] == 1

entry part2 (input: [][]i32) =
  let areas = map to_area input
  let grid = grid_claims areas
  in filter (has_sole_claim grid) areas |> head |> (.id)
