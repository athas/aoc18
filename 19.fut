-- This was a strange task.  I solved it by reverse-engineering the
-- machine code by hand.

type registers = {r0: i64, r1: i64, r2: i64, r3: i64, r4: i64, r5: i64}

type op = #addr | #addi
        | #mulr | #muli
        | #banr | #bani
        | #borr | #bori
        | #setr | #seti
        | #gtir | #gtri | #gtrr
        | #eqir | #eqri | #eqrr

type instruction = { op: op, a: u32, b: u32, c: u32 }

let fetch (i: u32) (r: registers): i64 =
  match i case 0 -> r.r0
          case 1 -> r.r1
          case 2 -> r.r2
          case 3 -> r.r3
          case 4 -> r.r4
          case _ -> r.r5

let set (i: u32) (v: i64) (r: registers): registers =
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
                   case #addi -> x + i64.u32 b
                   case #mulr -> x * y
                   case #muli -> x * i64.u32 b
                   case #banr -> x & y
                   case #bani -> x & i64.u32 b
                   case #borr -> x | y
                   case #bori -> x | i64.u32 b
                   case #setr -> x
                   case #seti -> i64.u32 a
                   case #gtir -> i64.bool (i64.u32 a > y)
                   case #gtri -> i64.bool (x > i64.u32 b)
                   case #gtrr -> i64.bool (x > y)
                   case #eqir -> i64.bool (i64.u32 a == y)
                   case #eqri -> i64.bool (x == i64.u32 b)
                   case #eqrr -> i64.bool (x == y)
  in set c v r

type~ machine = { ipr: u32, r: registers, code: []instruction }

let get_ip (m: machine) = fetch m.ipr m.r
let set_ip (m: machine) x = m with r = set m.ipr x m.r
let incr_ip (m: machine) = get_ip m |> (1+) |> set_ip m

let step (m: machine): machine =
  let instr = m.code[get_ip m]
  let m = m with r = exec instr.op instr.a instr.b instr.c m.r
  in incr_ip m

let run (m: machine): machine =
  loop m while get_ip m >= 0 && get_ip m < length m.code do step m

let parse (l: []i64): instruction =
  {op=all_ops[l[0]], a=u32.i64 l[1], b=u32.i64 l[2], c=u32.i64 l[3]}

let new_machine ipr code: machine =
  {ipr=u32.i64 ipr, r={r0=0, r1=0, r2=0, r3=0, r4=0, r5=0}, code}

entry part1 (ipr: i64) (input: [][]i64) =
  let code = map parse input
  let m = run (new_machine ipr code)
  in (m.r.r0, m.r.r1, m.r.r2, m.r.r3, m.r.r4, m.r.r5)

let step_until_r0_increased (m: machine) =
  let r0 = m.r.r0
  in loop (m, i) = (m, 0i64) while m.r.r0 == r0 do (step m, i+1)

entry steps (steps: i32) (ipr: i64) (input: [][]i64) =
  let code = map parse input
  let m0 = new_machine ipr code with r.r0 = 1
  let m = iterate steps step m0
  in (m.r.r0,
      m.r.r1,
      m.r.r2,
      m.r.r3,
      m.r.r4,
      m.r.r5)

-- This is the program I used to identify the main loop in the code.
entry codehisto (steps: i64) (ipr: i64) (input: [][]i64) =
  let code = map parse input
  let history = replicate (length input) 0i64
  let m = new_machine ipr code with r.r0 = 1
  let (history, _) = loop (history, m) for _i < steps do
                     let history[get_ip m] = history[get_ip m] + 1
                     in (history, step m)
  in history

-- You'll need to find y on your own.
entry part2 (y: i64) = map (\x -> if y % x == 0 then x else 0) (1...y) |> i64.sum
