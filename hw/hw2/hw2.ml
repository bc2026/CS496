(* Bhagawat Chapagain
	Hw2
	I pledge my honor that I have abided by the Stevens Honors System. 
*)
type 'a gt = Node of 'a *( 'a gt ) list

let t : int gt =
        Node (33,
                [Node (12,[]);
                 Node (77,
                        [Node (37,
                                [Node (14, [])]);
                                Node(48, []);
                                Node (103, [])])
                ])

let mk_leaf : 'a -> 'a gt = 
        fun n -> Node(n,[])


let rec list_max lst = match lst with
| [] -> 0
| h::t -> List.fold_left max h t       

let rec height : 'a gt -> int = function
  | Node(_, []) -> 1    
  | Node(_, children) -> 1 + list_max (List.map height children)

let rec size : 'a gt -> int = function
  | Node(_, []) -> 1
  | Node(_, children) -> 1 + List.fold_left (fun acc child -> acc + size child) 0 children

let rec paths_leaves_helper path t = match t with
    | Node (_, []) -> [List.rev path]  (* Leaf node: no children *)
    | Node (_, children) ->
        List.flatten (List.mapi (fun i child -> paths_leaves_helper (i :: path) child) children)
let paths_to_leaves t = paths_leaves_helper [] t

let is_leaf_perfect t =
  let rec get_leaf_depths t =
    match t with
    | Node (_, []) -> [0]  (* Leaf node: depth is 0 *)
    | Node (_, children) ->
        List.map ((+) 1) (List.flatten (List.map get_leaf_depths children))
  in
  let leaf_depths = get_leaf_depths t in
  match leaf_depths with
  | [] -> true (* Tree is empty, technically all leaves are at depth 0 *)
  | hd :: tl -> List.for_all ((=) hd) tl


let rec mirror t =
  match t with
  | Node (v, children) ->
      Node (v, List.map mirror (List.rev children))


let rec degree_helper t = 
  match t with
  | Node (_, children) ->
    let max_degree_child = List.fold_left (fun acc child ->
      max acc (degree_helper child)) 0 children in
    max (List.length children) max_degree_child

let rec degree =
	let t : int gt =
        Node (33,
                [Node (12,[]);
                 Node (77,
                        [Node (37,
                                [Node (14, [])]);
                                Node(48, []);
                                Node (103, [])])
                ]) in 
	degree_helper t


let rec preorder t =
  match t with
  | Node (v, children) -> v :: List.flatten (List.map preorder children)
let rec fold f t =
  match t with
  | Node (v, children) ->
      f v (List.map (fold f) children)

let sum t = 
        fold (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t

 let mem t e = 
         fold (fun i rs -> i=e || List.exists (fun i -> i) rs) t
		 
let mirror' : 'a gt -> 'a gt =
  let n d children =
    Node(d, List.rev children)
  in fold n


let rec map f t =
  match t with
  | Node (v, children) ->
      Node (f v, List.map (map f) children)
