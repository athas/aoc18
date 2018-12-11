-- This brute-force solution is crazy inefficient, but Futhark makes
-- it work.  I cannot believe this is how it's supposed to be done.

let hundreds_digit (x: i32) = (x % 1000) / 100

let power_level (serial: i32) (x: i32) (y: i32) =
  let foo = x * x * y + 20 * x * y + serial * x + 100 * y + 10 * serial
  in hundreds_digit foo - 5

let sum_square_at serial d x0 y0 =
  loop v = 0 for x in x0..<x0+d do
  loop v for y in y0..<y0+d do
  v + power_level serial x y

let square_total (serial: i32) (x: i32, y: i32) =
  sum_square_at serial 3 x y

let tag f x = (x, f x)

let maximum_by_key f x =
  let op x y = if i32.(f x < f y) then y else x
  in reduce_comm op x

entry part1 serial =
  tabulate_2d 298 298 (\x y -> (x+1, y+1))
  |> flatten
  |> map (tag (square_total serial))
  |> maximum_by_key (.2) ((-1,-1), i32.lowest)
  |> (.1)

let greatest_square_at area x0 y0 =
  loop (max_d, v) = (0, 0i32) for d < i32.min (300-x0) (300-y0) do
  let k = unsafe area[x0, y0] + area[x0+d, y0+d] - area[x0+d, y0] - area[x0, y0+d]
  in if k > v then (d, k) else (max_d, v)

entry part2 serial =
  let power = tabulate_2d 300 300 (\x y -> (x+1, y+1)) |> map (map (uncurry (power_level serial)))
  let area = power
             |> map (scan (+) 0)
             |> transpose
             |> map (scan (+) 0)
             |> transpose
  let ((x,y), (d, _)) =
    tabulate_2d 299 299 (\x y -> ((x+2,y+2), greatest_square_at area x y))
    |> flatten |> maximum_by_key (\(_, (_, x)) -> x) ((-1,-1), (0, 0))
  in (x, y, d)
