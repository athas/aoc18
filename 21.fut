-- This was also a strange task.  I solved it by reverse-engineering
-- the machine code by hand.

type registers = {r0: i32, r1: i32, r2: i32, r3: i32, r4: i32, r5: i32}

type op = #addr | #addi
        | #mulr | #muli
        | #banr | #bani
        | #borr | #bori
        | #setr | #seti
        | #gtir | #gtri | #gtrr
        | #eqir | #eqri | #eqrr

type instruction = { op: op, a: u32, b: u32, c: u32 }

let fetch (i: u32) (r: registers): i32 =
  match i case 0 -> r.r0
          case 1 -> r.r1
          case 2 -> r.r2
          case 3 -> r.r3
          case 4 -> r.r4
          case _ -> r.r5

let set (i: u32) (v: i32) (r: registers): registers =
  match i case 0 -> r with r0 = v
          case 1 -> r with r1 = v
          case 2 -> r with r2 = v
          case 3 -> r with r3 = v
          case 4 -> r with r4 = v
          case _ -> r with r5 = v

let all_ops: []op =
  [ #addr, #addi
  , #mulr, #muli
  , #banr, #bani
  , #borr, #bori
  , #setr, #seti
  , #gtir, #gtri, #gtrr
  , #eqir, #eqri, #eqrr]

let exec (op: op) (a: u32) (b: u32) (c: u32) (r: registers): registers =
  let x = fetch a r
  let y = fetch b r
  let v = match op case #addr -> x + y
                   case #addi -> x + i32.u32 b
                   case #mulr -> x * y
                   case #muli -> x * i32.u32 b
                   case #banr -> x & y
                   case #bani -> x & i32.u32 b
                   case #borr -> x | y
                   case #bori -> x | i32.u32 b
                   case #setr -> x
                   case #seti -> i32.u32 a
                   case #gtir -> i32.bool (i32.u32 a > y)
                   case #gtri -> i32.bool (x > i32.u32 b)
                   case #gtrr -> i32.bool (x > y)
                   case #eqir -> i32.bool (i32.u32 a == y)
                   case #eqri -> i32.bool (x == i32.u32 b)
                   case #eqrr -> i32.bool (x == y)
  in set c v r

type machine = { ipr: u32, r: registers, code: []instruction }

let get_ip (m: machine) = fetch m.ipr m.r
let set_ip (m: machine) x = m with r = set m.ipr x m.r
let incr_ip (m: machine) = get_ip m |> (1+) |> set_ip m

let step (m: machine): machine =
  let instr = unsafe m.code[get_ip m]
  let m = m with r = exec instr.op instr.a instr.b instr.c m.r
  in incr_ip m

let run (m: machine) (max_steps: i32): (machine, i32) =
  loop (m, i) = (m, 0) while get_ip m >= 0 && get_ip m < length m.code && i < max_steps do (step m, i + 1)

let parse (l: []i32): instruction =
  unsafe {op=all_ops[l[0]], a=u32.i32 l[1], b=u32.i32 l[2], c=u32.i32 l[3]}

let new_machine ipr code: machine =
  {ipr=u32.i32 ipr, r={r0=0, r1=0, r2=0, r3=0, r4=0, r5=0}, code}

-- (index, value)
let argmin [n] (xs: [n]i32): (i32, i32) =
  let f (i, x) (j, y) =
    if x > y then (j, y)
    else if x < y then (i, x)
    else if i > j then (j, y)
    else (i, x)
  in reduce_comm f (n, i32.highest) (zip (iota (length xs)) xs)

let argmax [n] (xs: [n]i32): (i32, i32) =
  let f (i, x) (j, y) =
    if y > x then (j, y)
    else if y < x then (i, x)
    else if i > j then (j, y)
    else (i, x)
  in reduce_comm f (-1, 0) (zip (iota (length xs)) xs)

-- Brute force!  Use a beefy GPU for this.
entry part1 (ipr: i32) (input: [][]i32) =
  let code = map parse input
  let max_steps = 4000
  let range = 20000000
  let m = map (\i -> run (new_machine ipr code with r.r0 = i) max_steps) (iota range)
  in map (.2) m |> argmin |> (.1)

-- For part2, use clever analysis that there is no room for in this
-- margin.
