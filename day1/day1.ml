#require "core";;
open Core
let (a, b) = List.map ~f:(fun (s) -> let l = String.lsplit2 ~on:' ' s in Option.value_map l ~default:(0, 0) ~f:(fun (a, b) -> (Int.of_string a, Int.of_string b))) (In_channel.read_lines "input.txt") |> List.unzip
(* let asort = List.sort ~compare:Int.compare a 
let bsort = List.sort ~compare:Int.compare b 
let rec do_stuff a b acc = 
  match (a, b) with
  | ([], []) -> acc 
  | (x::xs, y::ys) -> do_stuff xs ys (acc + Int.abs(y - x))
let ans = do_stuff asort bsort 0 *)
let rec do_more_stuff a b acc = 
  match a with
  | [] -> acc 
  | x::xs -> 
      let rec count_occurrences elem b count = 
        match b with
        | [] -> count 
        | b::bs -> if elem = b then count_occurrences elem bs (count + 1) else count_occurrences elem bs count 
      in 
      do_more_stuff xs b (acc + x * count_occurrences x b 0)
let ans2 = do_more_stuff a b 0 