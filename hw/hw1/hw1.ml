(* Bhagawat Chapagain
   CS 496 HW1
   I pledge my honor that I have abided by the Stevens Honor System *)

let e = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]
let square = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]

let mirror_helper item =
  match item with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 4
  | 3 -> 5
  | 4 -> 2
  | 5 -> 3
  | _ -> failwith "mirror_image: invalid input"

let mirror_image lst = List.map mirror_helper lst

let ninety_helper item = 
  match item with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 2
  | _ -> failwith "rotate_90_letter: invalid input"

let rotate_90_letter lst = List.map ninety_helper lst

let rec rotate_90_word lst = 
  match lst with
  | [] -> []
  | h::t -> rotate_90_letter h :: rotate_90_word t

let rec repeat n j =
  match n with
  | 0 -> []
  | n -> if n > 0 then j :: repeat (n-1) j
         else failwith "repeat: n is not an positive int"

let pantograph n p =
  List.flatten (List.map (fun x -> if x = 0 || x = 1 then [x] else repeat n x) p)

let pantograph_nm n p =
  List.flatten (List.map (fun x -> if x = 0 || x = 1 then [x] else repeat n x) p)

let pantograph_f n p =
  List.fold_right (fun x acc -> if x = 0 || x = 1 then x :: acc else (repeat n x) @ acc) p []

let travel curr dir =
        match dir with
        | 0 -> (fst curr, snd curr)
        | 1 -> (fst curr, snd curr)
        | 2 -> (fst curr, snd curr+1)
        | 4 -> (fst curr, snd curr-1)
        | 3 -> (fst curr + 1, snd curr)
        | 5 -> (fst curr - 1, snd curr)
        | _ -> (fst curr, snd curr)
let rec coverage start shape =
  match shape with
  | [] -> []
  | h::t -> if(h=0) then start:: start:: coverage start t
  else travel start h :: coverage (travel start h) t
  
let rec compress_helper acc current count = function
  | [] -> List.append acc [(current, count)]  (* Append the last counted element *)
  | h' :: t' when h' = current -> compress_helper acc current (count + 1) t'
  | h' :: t' -> compress_helper (List.append acc [(current, count)]) h' 1 t'

let rec compress_helper2 lst =
  match lst with
  | [] -> []
  | [x] -> [(x, 1)]  (* Single element case *)
  | h :: t -> compress_helper [] h 1 t

let rec compress lst = 
	compress_helper2 lst
  
let rec uncompress lst =
  match lst with
  | [] -> []
  | (command, count) :: rest ->
    let expanded_command = List.init count (fun _ -> command) in
    expanded_command @ uncompress rest


let expand (x, n) = List.init n (fun _ -> x)

let uncompress_m lst =
  List.flatten (List.map expand lst)

let uncompress_f lst =
  List.fold_left (fun acc (x, n) -> acc @ (repeat n x)) [] lst

let rec optimize_helper p acc = function
    | [] -> List.rev acc
    | x :: xs ->
        if (x = 1 && p) then helper p acc xs
        else if (x = 0 && not p) then helper p acc xs
        else helper (not p) (x :: acc) xs
let optimize p = optimize_helper true [] p
