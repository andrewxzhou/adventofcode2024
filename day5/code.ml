#require "core";;
open Core
let rules = List.map ~f:(fun s -> let l = String.split ~on:'|' s in (fun (x::y::[]) -> (x, y)) (List.map ~f:Int.of_string l)) (In_channel.read_lines "rules.txt") 
(* represent graph as a dict mapping ints (vertices) to seqs (neighbors) *)
(* run DFS and return reverse finish - copy over reverse finish from daniels notes lol*)
(* in theory, we shouldnt have any multi components. so this should work?*)
(* then we have a list of vertices in topological order. fold over this list. nil case - return report = nil. x::xs case. if x = y (first elem in report), consume both x and y. else only consume x. *)
let graph = Hashtbl.of_alist_multi ~growth_allowed:false ~size:100 (module Int) rules
(* X needs to be a set, and we need to use adjacency sequences *)
let rec listmem l v = 
  match l with
  | [] -> false 
  | x::xs -> v = x || listmem xs v 

let rec reverseFinish g l (sigma, x) v = 
  if Set.mem x v ||  not (listmem l v) then (sigma, x) else 
    let  x' = Set.union x (Set.singleton (module Int) v) in
    let neighbors = 
      match Hashtbl.find g v with
      | Some l -> l 
      | None -> [] in 
    let (sigma', x'') = List.fold_left neighbors ~init:(sigma, x') ~f:(reverseFinish g l) in (v::sigma', x'')
(* let (top_order, _) = List.fold_left (Hashtbl.keys graph) ~f:(reverseFinish graph) ~init:([], Set.empty (module Int)) *)
let reports = List.map ~f:(fun s -> let l = String.split ~on:',' s in  (List.map ~f:Int.of_string l)) (In_channel.read_lines "reports.txt") 
let rec consume l1 l2 = 
  (* l1 is top ordering, l2 is report *)
  match (l1, l2) with
  | (_, []) -> true 
  | ([], _) -> false 
  | (x::xs, y::ys) -> if x = y then consume xs ys else consume xs (y::ys)
let rec consume' orig l1 l2 = 
  (* l1 is top ordering, l2 is report *)
  match (l1, l2) with
  | (_, []) -> None 
  | ([], _) -> Some orig
  | (x::xs, y::ys) -> if x = y then consume' orig xs ys else consume' orig xs (y::ys)
let reports' = 
  List.reduce (List.map 
  (List.filter reports 
  ~f:(fun l -> 
    let (top_order, _) = reverseFinish graph l ([], Set.empty (module Int)) (List.nth_exn l 0) in consume top_order l)) ~f:(fun l -> List.nth_exn l (List.length l / 2))) (fun a -> fun b -> a + b)
let reports'' = 
List.reduce (List.map 
(List.filter_map reports 
~f:(fun l -> 
  let (top_order, _) = List.fold_left l ~init:([], Set.empty (module Int)) ~f:(reverseFinish graph l) in consume' top_order top_order l)) ~f:(fun l -> List.nth_exn l (List.length l / 2))) (fun a -> fun b -> a + b)
(* need to DFSAll*)