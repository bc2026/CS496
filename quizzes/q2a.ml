(* 
   Quiz 2 - 7 Feb 2022
   Name1: Bhagawat Chapagain 
   Name2: Joshua George
*)

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let t1 = Node(30,Node(20,Node(10,Empty,Empty),Empty),Empty)
let t2 = Node(4,
              Node(3,
                   Node(2,
                        Node(1,Empty,Empty),
                        Empty),
                   Empty),
              Node(5,
                   Empty,
                   Node(6,
                        Empty,
                        Node(7,Empty,Empty))))

let t3 = Node(12,
              Node(7,Empty,Empty),
              Node(24,
                   Node(18,Empty,Empty),
                   Empty))

(** Implement [level t i] which returns a list with all the nodes in
   [t] at level [i]. You may assume that [i] is greater than 0.
   1 is the first level. 
   If the level is greater than the height of the tree, then 
   it should return the empty list.
   Eg.
# level t2 1 ==> [4]
# level t2 2 ==> [3; 5]
# level t2 3 ==> [2; 6] 
# level t2 33 ==> [] 
**)

let rec level t i = match t with
  | Empty -> []
  | Node(v, l, r) ->
      if i = 1 then [v]
      else level l (i-1) @ level r (i-1)

(** Implement [levels t] which returns a list of lists, namely a list of the lists of nodes at each level. More precisely, the list at index i consists of all the items in [t] at level i+1.
   Eg. # levels t2 ==> [[4]; [3; 5]; [2; 6]; [1; 7]]
*)

  let rec levels_helper t n =
    let lvl = level t n in
    if lvl = [] then [] else lvl :: levels_helper t (n + 1)
let levels t =
  levels_helper t 1

(** Implement [pbt h d] that generates a perfect binary tree of a given height whose nodes contain [d] as
    data. The height is [h] is an integer greater or equal to zero.
    Eg.
 pbt 3 "a" ==>
 Node ("a", Node ("a", Node ("a", Empty, Empty), Node ("a", Empty, Empty)),
 Node ("a", Node ("a", Empty, Empty), Node ("a", Empty, Empty)))
   Eg.
   # pbt 0 3 ==> Empty
*)

let rec pbt h d = match h with
  | 0 -> Empty
  | _ -> Node(d, pbt (h-1) d, pbt (h-1) d)


(** Implement [paths_to_leaves t] which returns a list with all the paths from the root to a leaf 
    Eg. 
    # paths_to_leaves t2 => [[0; 0; 0]; [1; 1; 1]] 
*)
                      let rec paths_to_leaves t =
  let rec aux path t = match t with
    | Empty -> []
    | Node(_, Empty, Empty) -> [List.rev path]  (* Only include path when it's a leaf node *)
    | Node(_, l, r) ->
        let left_paths = aux (0 :: path) l in  (* Continue down the left subtree *)
        let right_paths = aux (1 :: path) r in  (* Continue down the right subtree *)
        left_paths @ right_paths  (* Combine paths from left and right subtrees *)
  in aux [] t
 
(** Implement [paths t] which returns a list with all the paths from
    the root to any node. If the tree is empty, then paths returns the
    empty list. (extra-credit)
    Eg. 
    # paths t2 => [[0; 0; 0]; [0; 0]; [0]; [1; 1; 1]; [1; 1];
    [1]; []]    
*)

 let rec path_helper path t = match t with
    | Empty -> [List.rev path]  (* Include path for the current node *)
    | Node(_, l, r) ->
        (* Include current node in path and continue with children *)
        let current_path = [List.rev path] in
        let left_paths = if l <> Empty then path_helper (0 :: path) l else [] in
        let right_paths = if r <> Empty then path_helper (1 :: path) r else [] in
        current_path @ left_paths @ right_paths
let rec paths t = path_helper [] t
