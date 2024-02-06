let rec sub l1 l2 =
  match l1 with
  | [] -> []
  | h::t -> 
      if (List.mem h l2)
        then sub t l2
      else h :: sub t l2


let rec outgoing_nodes t n = 
        match t with
        | [] -> []
        | (x,y)::t -> if (x=n) then y :: outgoing_nodes t n
        else outgoing_nodes t n;;


let add_if_not_exists lst elem =
  if List.mem elem lst then lst else elem :: lst

let rec nodes_helper t x =
  match t with
  | [] -> x  
  | (parent, child) :: r ->
      let new_x = add_if_not_exists x parent in  
      let new_x_with_child = add_if_not_exists new_x child in 
      nodes_helper r new_x_with_child

let rec nodes t = 
        nodes_helper t []

type tree = (int * int) list

let rec contains lst x = match lst with
  | [] -> false
  | h :: t -> if h = x then true else contains t x

let rec leaves t =
  let rec helper t acc_parents acc_children = match t with
    | [] -> List.filter (fun child -> not (contains acc_parents child)) acc_children
    | (parent, child) :: rest ->
      helper rest (parent :: acc_parents) (child :: acc_children)
  in
  helper t [] []

let rec root t =
  let rec helper t acc_parents acc_children = match t with
    | [] -> List.filter (fun parent -> not (contains acc_children parent)) acc_parents
    | (parent, child) :: rest ->
      helper rest (parent :: acc_parents) (child :: acc_children)
  in
  match helper t [] [] with
  | [] -> failwith "Tree is empty or invalid"
  | h :: _ -> [h]

let rec is_binary t =
  let rec count_children t map = match t with
    | [] -> List.for_all (fun (_, count) -> count <= 2) map
    | (parent, _) :: rest ->
      let new_map = if List.mem_assoc parent map then
                      (parent, (List.assoc parent map) + 1) :: (List.remove_assoc parent map)
                    else
                      (parent, 1) :: map in
      count_children rest new_map
  in
  count_children t []

let rec subtree t n =
  (* Helper function to find immediate children of a node *)
  let rec find_children t n = match t with
    | [] -> []
    | (parent, child) :: rest ->
        if parent = n then child :: find_children rest n
        else find_children rest n
  in
  (* Recursive function to build the subtree *)
  let rec build_subtree t n = match find_children t n with
    | [] -> []
    | children ->
        List.fold_left (fun acc child ->
          acc @ [(n, child)] @ build_subtree t child
        ) [] children
  in
  build_subtree t n

