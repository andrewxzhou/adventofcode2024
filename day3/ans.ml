#require "core";;
open Core
type state = M | U | L | PAREN_LEFT | COMMA | NUMA | NUMB | NORM
type state2 = D | O | LPAREN | N | APO | T | NORM'
type state3 = ENABLED | DISABLED
let mul = String.concat (In_channel.read_lines "input.txt") |> String.to_list
let charlist_to_int = fun (l) -> List.rev l |> String.of_char_list |> Int.of_string
let rec consume l acc a b state = 
  match l with
  | [] -> acc 
  | x::xs -> 
    (match state with
    | NORM -> 
      (match x with
      | 'm' -> consume xs acc [] [] M
      | _ -> consume xs acc [] [] NORM)
    | M -> 
      (match x with
      | 'u' -> consume xs acc [] [] U
      | _ -> consume xs acc [] [] NORM)
    | U -> 
      (match x with
      | 'l' -> consume xs acc [] [] L
      | _ -> consume xs acc [] [] NORM)
    | L -> 
      (match x with
      | '(' -> consume xs acc [] [] PAREN_LEFT
      | _ -> consume xs acc [] [] NORM)
    | PAREN_LEFT -> 
      if Char.is_digit x then consume xs acc (x::a) b NUMA else consume xs acc [] [] NORM
    | NUMA -> 
      if Char.is_digit x then consume xs acc (x::a) b NUMA else if Char.equal x ',' then consume xs acc a b COMMA else consume xs acc [] [] NORM
    | COMMA -> 
      if Char.is_digit x then consume xs acc a (x::b) NUMB else consume xs acc [] [] NORM
    | NUMB -> 
      if Char.is_digit x then consume xs acc a (x::b) NUMB else if Char.equal x ')' then consume xs (acc + (charlist_to_int a) * (charlist_to_int b)) [] [] NORM else consume xs acc [] [] NORM)
let ans = consume mul 0 [] [] NORM 
let rec consumedo l acc state en = 
  match l with
  | [] -> List.rev acc 
  | x::xs -> 
    (match en with
    | ENABLED -> 
      (match state with
      | NORM' -> 
        (match x with
        | 'd' -> consumedo xs (x::acc) D ENABLED
        | _ -> consumedo xs (x::acc) NORM' ENABLED)
      | D -> 
        (match x with
        | 'o' -> consumedo xs (x::acc) O ENABLED
        | _ -> consumedo xs (x::acc) NORM' ENABLED)
      | O -> 
        (match x with
        | 'n' -> consumedo xs (x::acc) N ENABLED
        | _ -> consumedo xs (x::acc) NORM' ENABLED)
      | N -> 
        (match x with
        | '\'' -> consumedo xs (x::acc) APO ENABLED
        | _ -> consumedo xs (x::acc) NORM' ENABLED)
      | APO -> 
        (match x with
        | 't' -> consumedo xs (x::acc) T ENABLED
        | _ -> consumedo xs (x::acc) NORM' ENABLED)
      | T -> 
        (match x with
        | '(' -> consumedo xs (x::acc) LPAREN ENABLED
        | _ -> consumedo xs (x::acc) NORM' ENABLED)
      | LPAREN -> 
        (match x with
        | ')' -> consumedo xs (x::acc) NORM' DISABLED
        | _ -> consumedo xs (x::acc) NORM' ENABLED))

    | DISABLED -> 
      (match state with
      | NORM' -> 
        (match x with
        | 'd' -> consumedo xs acc D DISABLED
        | _ -> consumedo xs acc NORM' DISABLED)
      | D -> 
        (match x with
        | 'o' -> consumedo xs acc O DISABLED
        | _ -> consumedo xs acc NORM' DISABLED)
      | O -> 
        (match x with
        | '(' -> consumedo xs acc LPAREN DISABLED
        | _ -> consumedo xs acc NORM' DISABLED)
      | LPAREN -> 
        (match x with
        | ')' -> consumedo xs acc NORM' ENABLED
        | _ -> consumedo xs acc NORM' DISABLED)))
let ans2 = consume (consumedo mul [] NORM' ENABLED) 0 [] [] NORM 
let dos = consumedo mul [] NORM' ENABLED |> String.of_char_list
    