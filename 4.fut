-- We disregard the year.
type time = i32

let time_from_record {month: i32, day: i32, hour: i32, minute: i32} =
  month * 1000000 + day * 10000 + hour * 100 + minute

type event = {time: time, what: i32}

let event_from_raw (raw: []i32): event =
  {time=time_from_record {month=raw[1], day=raw[2], hour=raw[3], minute=raw[4]},
   what = raw[5]}

-- We annotate the wakeup/sleep events with guard IDs.  Specifically,
-- of the i32 denoting an event, the lower 16 bits indicate the ID of
-- the guard, and the integer value of the upper 16 bits the event:
--
-- 0: begins shift.
-- 1: falls asleep.
-- 2: wakes up.

type guard_event = #begins_shift | #falls_asleep | #wakes_up
type guard_id = i32

let guard_event' (what: i32): (guard_event, guard_id) =
  let guard = what & 0xFFFF
  let event = if what >> 16 == 0 then #begins_shift
              else if what >> 16 == 1 then #falls_asleep
              else #wakes_up
  in (event, guard)

let guard_event (e: event) = guard_event' e.what

let annotate_guards (es: []event): []event =
  let times = map (.time) es
  let infer e1 e2 =
    let (_, e1_g) = guard_event' e1
    let (_, e2_g) = guard_event' e2
    in if e2_g == 0 then e2 | e1_g
       else e2
    let annotated = scan infer 0 (map (.what) es)
    in map2 (\time what -> {time, what}) times annotated

import "lib/github.com/diku-dk/sorts/radix_sort"

let events_from_raws (raws: [][]i32) =
  raws
  |> map event_from_raw
  |> radix_sort_by_key (.time) i32.num_bits i32.get_bit
  |> annotate_guards

type sleep = {who: guard_id, from: i32, to: i32}

let sleep_duration ({who=_, from, to}: sleep) =
  to - from

let in_sleep (m: i32) ({who=_, from, to}: sleep): bool =
  m >= from%100 && m < to%100

let sleeps (es: []event): []sleep =
  let check_sleep x y =
    let (x_e, x_id) = guard_event x
    let (y_e, _) = guard_event y
    in if x_e == #falls_asleep && y_e == #wakes_up
       then {who=x_id, from=x.time, to=y.time}
       else {who= -1, from=0, to=0}
  in map2 check_sleep es (rotate 1 es)

let minutes_histogram (ss: []sleep): [60]i32 =
  tabulate 60 (\m -> map (in_sleep m) ss |> map i32.bool |> i32.sum)

-- (index, value)
let argmax [n] (xs: [n]i32): (i32, i32) =
  let f (i, x) (j, y) =
    if x > y then (i, x)
    else if y < x then (j, y)
    else if i > j then (i, x)
    else (j, y)
  in reduce_comm f (-1, 0) (zip (iota n) xs)

let max_guard_id: i32 = 4000 -- ckjaer did it first

let full_minutes_histogram (ss: []sleep): [max_guard_id][60]i32 =
  let for_guard id =
    ss
    |> map (\s -> if s.who == id then s else {who=0, from=0, to=0})
    |> minutes_histogram
  in tabulate max_guard_id for_guard

entry part1 (raws: [][]i32) =
  let events = events_from_raws raws
  let sleeps = sleeps events
  let sleep_minutes =
    reduce_by_index (replicate max_guard_id 0) (+) 0
    (map (.who) sleeps) (map sleep_duration sleeps)
  let (most_sleepy, _) = argmax sleep_minutes
  let most_sleepy_sleeps = filter ((.who) >-> (==most_sleepy)) sleeps
  in (most_sleepy * (most_sleepy_sleeps |> minutes_histogram |> argmax |> (.0)))

entry part2 (raws: [][]i32) =
  let events =
    events_from_raws raws
  let sleeps = sleeps events
  let guards_to_minute_focus = full_minutes_histogram sleeps |> map argmax
  let most_focused = map (.1) guards_to_minute_focus |> argmax |> (.0)
  in (most_focused * guards_to_minute_focus[most_focused].0)
