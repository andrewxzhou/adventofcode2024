#require "core";;
open Core
let levels = List.map ~f:(fun (s) -> let l = String.split ~on:' ' s in List.map ~f:Int.of_string l) (In_channel.read_lines "input.txt") 
let rec check_level l = 
  match l with
  | [] -> true 
  | x::[] -> true 
  | x::y::xs -> let diff = y - x in if diff <= 3 && x < y && diff >= 1 then check_level (y::xs) else false  
let rec check_level' l prev = 
  match l with
  | [] -> true 
  | x::[] -> true 
  | x::y::xs -> let diff = y - x in if diff <= 3 && x < y && diff >= 1 then check_level' (y::xs) (Some x) else match prev with
  | None -> check_level (y::xs) || check_level (x::xs)   
  | Some z -> check_level(z::y::xs) || check_level (x::xs)   
  
let rec do_stuff l acc test = 
  match l with
  | [] -> acc 
  | x::xs -> if check_level' x None || check_level' (List.rev x) None then (Printf.printf "%8d\n" test ; do_stuff xs (acc + 1) (test + 1)) else do_stuff xs acc (test + 1)
let ans = do_stuff levels 0 1