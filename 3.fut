-- For this one I got really lazy.  Very brute force.  Would scale
-- very poorly for a sparse grid.

type area = { id: i64, left: i64, top: i64, width: i64, height: i64 }

let in_area (x: i64) (y: i64) (area: area): bool =
  x >= area.left && x < area.left + area.width &&
  y >= area.top && y < area.top + area.height

let area_span_x (area: area) =
  area.left + area.width

let area_span_y (area: area) =
  area.top + area.height

let to_area (raw: []i64): area =
  {id=raw[0], left=raw[1], top=raw[2], width=raw[3], height=raw[4]}

let area_num_pixels (area: area) =
  area.width * area.height

let area_get_pixel (area: area) i =
  (i / area.width, i % area.width)

let area_get_pixel_flat max_x area i =
  let (x, y) = area_get_pixel area i
  in (x + area.top + (y + area.left) * max_x)

import "lib/github.com/diku-dk/segmented/segmented"

let grid_claims areas: [][]i64 =
  let max_x = map area_span_x areas |> i64.maximum
  let max_y = map area_span_y areas |> i64.maximum
  let points = areas |> expand area_num_pixels (area_get_pixel_flat max_x)
  in reduce_by_index (replicate (max_x*max_y) 0) (+) 0 points (map (const 1) points)
     |> unflatten max_x max_y

entry part1 (input: [][]i64) =
  let areas = map to_area input
  let occupied = grid_claims areas |> flatten
  in occupied |> map (>1) |> map i64.bool |> i64.sum

let has_sole_claim (grid: [][]i64) (area: area): bool =
  loop ok = true for x in area.left..<area.left+area.width do
  loop ok for y in area.top..<area.top+area.height do
  ok && grid[x,y] == 1

entry part2 (input: [][]i64) =
  let areas = map to_area input
  let grid = grid_claims areas
  in filter (has_sole_claim grid) areas |> head |> (.id)
