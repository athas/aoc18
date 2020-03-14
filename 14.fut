-- This one is also not parallel at all, but it is OK I guess.  AT
-- least the code is harmless.

let brew (k: i32) =
  let elves = (0,1)
  let recipes = [3,7] ++ replicate (k+10) 0i32
  let num_recipes = 2
  let (recipes, _, _) =
    loop (recipes, elves, num_recipes) while num_recipes < k+10 do
    let a = recipes[elves.0]
    let b = recipes[elves.1]
    let x = a + b
    let (recipes, num_recipes) =
      (if x >= 10 then let recipes[num_recipes] = x / 10
                       let recipes[num_recipes+1] = x % 10
                       in (recipes, num_recipes + 2)
                  else let recipes[num_recipes] = x % 10
                       in (recipes, num_recipes + 1))
    let elves = ((elves.0 + a + 1) % num_recipes,
                 (elves.1 + b + 1) % num_recipes)
    in (recipes, elves, num_recipes)
  in recipes

entry part1 (k: i32) =
  let recipes = brew k
  in recipes[k:k+10]

let digits (x: i32) =
  let num_digits = (loop (x,i) = (x,0) while x > 0 do (x/10, i+1)).1
  in map (\i -> (x / (10**i))%10) (reverse (iota num_digits))

let arreq [n] [m] (xs: [n]i32) (ys: [m]i32) : bool =
  n == m && xs == (ys :> [n]i32)

entry part2 (k: i32) =
  let recipes = brew 25000000
  let looking_for = digits k
  in loop i = 0 while recipes[i:i+length looking_for] `arreq` looking_for do i + 1
