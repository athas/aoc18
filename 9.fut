-- This is not parallel at all.
--
-- But it is how Knuth would do linked lists.

type link = {next: i64, prev: i64}

let insert_marble (where: i64) (i: i64) (links: *[]link): *[]link =
  let following_id = links[where].next
  let following = links[following_id]
  let following_following_id = following.next
  let links[i] = {prev=following_id, next=following.next}
  let links[following_id] = links[following_id] with next = i
  let links[following_following_id] = links[following_following_id] with prev = i
  in links

let remove_marble where (links: *[]link): (*[]link, i64) =
  let {prev=prev_id, next=next_id} = links[where]
  let prev = links[prev_id]
  let next = links[next_id]
  let links[prev_id] = prev with next = next_id
  let links[next_id] = next with prev = prev_id
  in (links, where)

let move where (n: i64) (links: []link): i64 =
  if n > 0
  then loop where for _i < n do links[where].next
  else loop where for _i < i64.abs n do links[where].prev

let game (num_players: i64) (highest_marble: i64) =
  let num_marbles = highest_marble + 1
  let links = replicate num_marbles {next=0, prev=0}
  let points = replicate num_players 0u64
  let cur = 0
  let (_, points, _) =
    (loop (links, points, cur) for i < num_marbles do
       if i % 23 == 0
       then let cur_player = i % num_players
            let to_remove = move cur (-7) links
            let cur = links[to_remove].next
            let (links, removed) = remove_marble to_remove links
            let points[cur_player] = points[cur_player] + u64.i64 i + u64.i64 removed
            in (links, points, cur)
       else let links = insert_marble cur i links
            in (links, points, i))
  in u64.maximum points

entry part1 = game

entry part2 num_players highest_marble = game num_players (highest_marble * 100)
