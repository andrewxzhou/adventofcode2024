#require "core";;
open Core
type step = | Increase | Decrease | Both  
let levels = List.map ~f:(fun (s) -> let l = String.split ~on:' ' s in List.map ~f:Int.of_string l) (In_channel.read_lines "input.txt") 
let rec check_level l s acc lacc = 
  match l with
  | [] -> (acc, lacc) 
  | x::[] -> (acc, x::lacc) 
  | x::y::xs -> match s with
    | Both -> (match compare x y with
      | 0 -> check_level (y::xs) Both (acc + 1) lacc
      | 1 -> let diff = Int.abs(y - x) in if diff <= 3 then check_level (y::xs) Decrease acc (x::lacc) else check_level (y::xs) Decrease (acc + 1) lacc
      | (-1) -> let diff = Int.abs(y - x) in if diff <= 3 then check_level (y::xs) Increase acc (x::lacc) else check_level (y::xs) Increase (acc + 1) lacc)
    | Increase -> let diff = Int.abs(y - x) in if diff <= 3 && x < y && diff >= 1 then check_level (y::xs) Increase acc (x::lacc) else check_level (y::xs) Increase (acc + 1) lacc 
    | Decrease -> let diff = Int.abs(x - y) in if diff <= 3 && y < x && diff >= 1 then check_level (y::xs) Decrease acc (x::lacc) else check_level (y::xs) Decrease (acc + 1) lacc 

let rec check_level2 l s acc = 
  match l with
  | [] -> acc
  | x::[] -> acc
  | x::y::xs -> match s with
    | Both -> (match compare x y with
      | 0 -> check_level2 (y::xs) Both (acc + 1) 
      | 1 -> let diff = Int.abs(y - x) in if diff <= 3 then check_level2 (y::xs) Decrease acc else check_level2 (y::xs) Decrease (acc + 1) 
      | (-1) -> let diff = Int.abs(y - x) in if diff <= 3 then check_level2 (y::xs) Increase acc  else check_level2 (y::xs) Increase (acc + 1)) 
    | Increase -> let diff = Int.abs(y - x) in if diff <= 3 && x < y && diff >= 1 then check_level2 (y::xs) Increase acc else check_level2 (y::xs) Increase (acc + 1)  
    | Decrease -> let diff = Int.abs(x - y) in if diff <= 3 && y < x && diff >= 1 then check_level2 (y::xs) Decrease acc else check_level2 (y::xs) Decrease (acc + 1) 

(*    let rec do_stuff l acc test = 
  match l with
  | [] -> acc 
  | x::xs -> if check_level x Both then let dbg = (Printf.printf "%8d\n" test) in do_stuff xs (acc + 1) (test + 1) else do_stuff xs acc (test + 1) *)

let potentials = List.map ~f:(fun l -> (check_level l Both 0 [], l)) levels 
let ones = List.filter ~f:(fun ((n, removed), l) -> if n = 1 && 0 = check_level2 removed Both 0  then true else false) potentials |> List.length 
let safe = List.filter ~f:(fun ((n, _), l) -> if n = 0 then true else false) potentials |> List.length 

let mountain x y z = if ((y > x && y > z) || (y < x && y < z)) && (Int.abs (x - z) <= 3 && Int.abs (x - z) >= 1) then true else false  
let rec check_twos l acc = 
  match l with
  | [] -> acc
  | x::[] -> acc 
  | x::y::[] -> acc 
  | x::y::z::xs -> if mountain x y z then check_twos xs (x::z::acc) else check_twos (y::z::xs) (x::acc)
let twos = List.filter ~f:(fun ((n, _), l) -> if n = 2 && 0 = check_level2 (check_twos l []) Both 0 then true else false) potentials |> List.length 
let ans = ones + safe + twos