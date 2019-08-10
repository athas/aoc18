-- This one is OK.  Part 2 is entirely sequential, of course.

type registers = {r0: i32, r1: i32, r2: i32, r3: i32}
type instruction = u32
type example = {bef: registers, op: instruction, aft: registers}

let instruction_opcode (x: instruction) = x>>24 & 0xFF
let instruction_a (x: instruction) = x>>16 & 0xFF
let instruction_b (x: instruction) = x>>8 & 0xFF
let instruction_c (x: instruction) = x & 0xFF
let mk_instruction op a b c: instruction = op << 24 | a << 16 | b << 8 | c

type op = #addr | #addi
        | #mulr | #muli
        | #banr | #bani
        | #borr | #bori
        | #setr | #seti
        | #gtir | #gtri | #gtrr
        | #eqir | #eqri | #eqrr

let all_ops: []op =
  [ #addr, #addi
  , #mulr, #muli
  , #banr, #bani
  , #borr, #bori
  , #setr, #seti
  , #gtir, #gtri, #gtrr
  , #eqir, #eqri, #eqrr]

let fetch (i: u32) (r: registers): i32 =
  match i case 0 -> r.r0
          case 1 -> r.r1
          case 2 -> r.r2
          case _ -> r.r3

let set (i: u32) (v: i32) (r: registers): registers =
  match i case 0 -> r with r0 = v
          case 1 -> r with r1 = v
          case 2 -> r with r2 = v
          case _ -> r with r3 = v

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

let example_valid (op: op) (ex: example): bool =
  exec op (instruction_a ex.op) (instruction_b ex.op) (instruction_c ex.op) ex.bef == ex.aft

let num_possibilities (ex: example): i32 =
  map (`example_valid` ex) all_ops |> map i32.bool |> i32.sum

let parse_registers l = {r0 = l[0], r1 = l[1], r2 = l[2], r3 = l[3]}
let parse = map (\line -> {bef= parse_registers line[0],
                           op= mk_instruction (u32.i32 line[1,0])
                                              (u32.i32 line[1,1])
                                              (u32.i32 line[1,2])
                                              (u32.i32 line[1,3]),
                           aft= parse_registers line[2]})

entry part1 (input: [][][]i32) (_: [][]i32) =
  let examples = parse input
  in examples |> map num_possibilities |> map (>=3) |> map i32.bool |> i32.sum

-- This function was used to compute the opcode table, but it does not
-- directly do it.
let opcode_possibilities (code: u32) (exs: []example) =
  let correct op ex = if instruction_opcode ex.op == code
                      then example_valid op ex
                      else true
  in copy all_ops |> filter (\op -> all (correct op) exs)

let optable: []op =
  [#bani
  ,#addr
  ,#mulr
  ,#addi
  ,#gtri
  ,#banr
  ,#borr
  ,#eqri
  ,#seti
  ,#eqrr
  ,#bori
  ,#setr
  ,#eqir
  ,#muli
  ,#gtrr
  ,#gtir]

entry verify (input: [][][]i32) (_: [][]i32) =
  let examples = parse input
  in examples |> map (\ex -> example_valid (optable[i32.u32 (instruction_opcode ex.op)]) ex)

entry part2 (_: [][][]i32) (code: [][]i32) =
  let instrs = map (\l -> mk_instruction (u32.i32 l[0])
                                         (u32.i32 l[1])
                                         (u32.i32 l[2])
                                         (u32.i32 l[3]))
                   code
  let exec' regs instr =
    exec optable[i32.u32 (instruction_opcode instr)]
         (instruction_a instr)
         (instruction_b instr)
         (instruction_c instr)
         regs
  let regs = foldl exec' {r0=0, r1=0, r2=0, r3=0} instrs
  in regs.r0
