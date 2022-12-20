-- This one goes beyond sucking.  It is a black hole of hacks.  Many
-- moons have passed since last I wrote a program that triggered so
-- many compiler bugs.  And there's not even any parallelism worth
-- speaking of in this problem!  Well, maybe some speculative
-- parallelism, but who has time for that?

-- Bit vector encoding
-- bit 0: set if cart is present
-- bit 1: set if track
-- bit 5: set if /
-- bit 6: set if \
-- bit 7: set if +
type cell = u8

let empty_cell: cell = 0
let cell_with_track: cell        = 0x00000010
let cell_with_cart: cell         = 0b00000011
let cell_with_fturn: cell        = 0b00100010
let cell_with_bturn: cell        = 0b01000010
let cell_with_intersection: cell = 0b10000010

type dir = #north | #east | #south | #west
type turn = #left | #straight | #right

let dir_from_num (x: u8): dir =
  match x % 4
  case 0 -> #north
  case 1 -> #east
  case 2 -> #south
  case _ -> #west

let dir_to_num (d: dir): u8 =
  match d
  case #north -> 0
  case #east -> 1
  case #south -> 2
  case #west -> 3

let turn_from_num (x: u8): turn =
  match x % 3
  case 0 -> #left
  case 1 -> #straight
  case _ -> #right

let turn_to_num (t: turn): u8 =
  match t
  case #left -> 0
  case #straight -> 1
  case #right -> 2

let next_turn (t: turn): turn =
  match t
  case #left -> #straight
  case #straight -> #right
  case #right -> #left

let turn (d: dir) (t: turn): dir =
  match (d, t)
  case (#south, #left) -> #east
  case (#south, #right) -> #west
  case (#north, #left) -> #west
  case (#north, #right) -> #east
  case (#east, #left) -> #north
  case (#east, #right) -> #south
  case (#west, #left) -> #south
  case (#west, #right) -> #north
  case (_, #straight) -> d

let move (x: i64, y: i64) (d: dir) =
  match d
  case #south -> (x+1, y)
  case #north -> (x-1, y)
  case #west -> (x, y-1)
  case #east -> (x, y+1)

let cell_track (x: cell) = (x >> 5)&1 == 1
let cell_intersection (x: cell) = (x >> 7)&1 == 1
let cell_has_cart (x: cell) = x & 1 == 1
let cell_add_cart (x: cell) = x | 1
let cell_remove_cart (x: cell) = x & -1u8
let is (x: cell) (y: cell) = (x & y) == y

import "lib/github.com/diku-dk/sorts/merge_sort"

type cart = (i64, i64, dir, turn)

let cartlte ((x1, y1, _, _): cart) ((x2, y2, _, _): cart): bool =
  x1 < x2 || x1 == x2 && y1 <= y2

let parse [n][m] (input: [n][m]i64): (*[n][m]cell, *[]cart) =
  let cell c = if c == ' ' then empty_cell
               else if c == '>' || c == '<' || c == '^' || c == 'v'
               then cell_with_cart
               else if c == '\\' then cell_with_bturn
               else if c == '/' then cell_with_fturn
               else if c == '+' then cell_with_intersection
               else cell_with_track
  let cells = map (map cell) input
  let cart i j = let c = input[i,j]
                 let d = if c == '<' then #west
                         else if c == '>' then #east
                         else if c == '^' then #north
                         else #south
                 in ((i,j,d,#left), cells[i,j] == cell_with_cart)
  let carts = tabulate_2d n m cart
              |> flatten |> filter (.1) |> map (.0) |> merge_sort cartlte
  in (cells, copy carts)

let cell_has_track [n][m] (cells: [n][m]cell) (x, y) =
  x >= 0 && x < n && y >= 0 && y < m && (cells[x,y] `is` cell_with_track)

let maybe_turn_cart cells ((x, y, d, t): cart): cart =
  let c = cells[x,y]
  let at_intersection = c `is` cell_with_intersection
  let at_turn = (c `is` cell_with_fturn) || (c `is` cell_with_bturn)
  let (d, t) =
    if at_intersection then (turn d t, next_turn t)
    else if at_turn then
          (match d
           case #north -> if c `is` cell_with_fturn then #east else #west
           case #south -> if c `is` cell_with_fturn then #west else #east
           case #east -> if c `is` cell_with_fturn then #north else #south
           case #west -> if c `is` cell_with_fturn then #south else #north,
           t)
    else (d, t)
  in (x, y, d, t)

let cart_pos (c: cart) = (c.0, c.1)

let move_cart [n][m] (cells: *[n][m]cell) ((x, y, d, t): cart): (*[n][m]cell, cart, bool) =
  let (x', y') = move (x, y) d
  let collision = cell_has_cart cells[x',y']
  in let a = cell_remove_cart cells[x,y] -- FIXME
     let b = cell_add_cart cells[x',y']
     let cells' = cells with [x,y] = a with [x', y'] = b
     in (cells', maybe_turn_cart cells' (x', y', d, t), collision)

entry part1 (max_steps: i64) (input: [][]i64) =
  let (cells, carts) = parse input
  let num_carts = length carts
  let (_, _, steps, collision) =
    loop (cells, carts, steps, collision) = (cells, carts, 0i64, (-1,-1))
    while collision == (-1,-1) && steps < max_steps do
    let (cells, carts, collision) =
      loop (cells, carts, collision) for i < num_carts do
      let (cells, cart, collides) = move_cart cells carts[i]
      let collision = if collides && collision == (-1, -1)
                      then cart_pos cart
                      else collision
      in (cells, carts with [i] = cart, collision)
    in (cells, merge_sort cartlte carts, steps + 1, collision)
  in (steps, collision.1, collision.0)

let nullify_carts_at p (cells: *[][]cell) (carts: *[]cart): (*[][]cell, *[]cart) =
  let num_carts = length carts in
  loop (cells : *[][]cell, carts : *[]cart) for i < num_carts do
  let (x,y) = cart_pos carts[i]
  in if (x,y) == p then let new_cell = cell_remove_cart cells[x,y]
                        let new_cart = (-1, -1, carts[i].2, carts[i].3) -- FIXME: use 'with'
                        in (cells with [x,y] = new_cell,
                            carts with [i] = new_cart)
    else (cells, carts)

entry part2 (max_steps: i64) (input: [][]i64) =
  let (cells, carts) = parse input
  let num_carts = length carts
  let (_, carts, steps, _) =
    loop (cells, carts, steps, carts_left) = (cells, carts, 0, num_carts)
    while carts_left > 1 && steps < max_steps do
    let (cells, carts, collision) =
      loop (cells, carts, carts_left) for i < num_carts do
      if cart_pos carts[i] == (-1, -1) then (cells, carts, carts_left)
      else let (cells, cart, collides) = move_cart cells carts[i]
           let carts[i] = cart
           let (cells, carts) = if collides
                                then nullify_carts_at (cart_pos cart) cells carts
                                else (cells, carts with [i] = cart)
           in (cells, carts, if collides then carts_left - 2 else carts_left)
    in (cells, merge_sort cartlte carts, steps + 1, collision)
  let (x,y) = cart_pos (last carts)
  in (steps,y,x)
