(* This file defines expressed values and environments *)
open Parser_plaf.Ast

(* operations on expressed values *)

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree


type exp_val =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string*expr*env    
  | UnitVal
  | ListVal of exp_val list
  | TreeVal of exp_val tree
  | TupleVal of exp_val list
  | RecordVal of (string*exp_val) list
and
   env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env


(* Environment Abstracted Result *)

type 'a result = Ok of 'a | Error of string

type 'a ea_result = env -> 'a result
  
let return (v:'a) : 'a ea_result =
  fun _env ->
  Ok v

let error (s:string) : 'a ea_result =
  fun _env ->
  Error s

let (>>=) (c:'a ea_result) (f: 'a -> 'b ea_result) : 'b ea_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok v -> f v env

let (>>+) (c:env ea_result) (d:'a ea_result): 'a ea_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok newenv -> d newenv

let run (c:'a ea_result) : 'a result =
  c EmptyEnv

let lookup_env : env ea_result =
  fun env ->
  Ok env

let rec sequence : ('a ea_result) list -> ('a list) ea_result  =
  fun cs ->
  match cs with
  | [] -> return []
  | c::t ->
    c >>= fun v ->
    sequence t >>= fun vs ->
    return (v::vs)
      
(* Operations on environments *)

let empty_env : unit -> env ea_result =
  fun () ->
    return EmptyEnv

let extend_env : string -> exp_val -> env ea_result =
  fun id v env ->
    Ok (ExtendEnv(id,v,env))

let rec apply_env : string -> exp_val ea_result =
  fun id env ->
  match env with
  | EmptyEnv -> Error (id^" not found!")
  | ExtendEnv(v,ev,tail) ->
    if id=v
    then Ok ev
    else apply_env id tail


let rec extend_env_list : string list -> exp_val list -> env ea_result = fun ids evs ->
  match ids,evs  with
  | [],[] -> lookup_env
  | id::ids, _ when List.mem id ids -> error "duplicate identifiers"
  | id::ids, ev::evs -> extend_env id ev >>+ extend_env_list ids evs
  | _,_ -> error "extend_env_list: Arguments do not match parameters!"

(* operations on expressed values *)

let int_of_numVal : exp_val -> int ea_result =  function
  |  NumVal n -> return n
  | _ -> error "Expected a number!"

let bool_of_boolVal : exp_val -> bool ea_result =  function
  |  BoolVal b -> return b
  | _ -> error "Expected a boolean!"

let list_of_listVal =  function
  | ListVal l -> return l
  | _ ->  error "Expected a list!"

let tree_of_treeVal =  function
  | TreeVal t -> return t
  | _ -> error "Expected a tree!"

let tuple_of_tupleVal : exp_val -> (exp_val list) ea_result = function
  | TupleVal(evs) -> return evs
  | _ -> error "Expected a tuple!"

let clos_of_procVal : exp_val -> (string*expr*env) ea_result =
  fun ev ->
  match ev with
  | ProcVal(id,body,en) -> return (id,body,en)
  | _ -> error "Expected a closure!"
           
let is_listVal = function
  | ListVal(_) ->  true
  | _ ->  false

let is_treeVal = function
  | TreeVal(_) ->  true
  | _ ->  false

let rec string_of_list_of_strings = function
  | [] -> ""
  | [id] -> id
  | id::ids -> id ^ "," ^ string_of_list_of_strings ids

let fields_of_recordVal = function
  | RecordVal fs -> return fs
  | _ -> error "Expected a record"


let rec has_dupl l =
  match l with
  | [] -> false
  | h::t ->
    List.mem h t || has_dupl t

let rec string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | UnitVal  -> "UnitVal"
  | ProcVal(par,body,env) -> "ProcVal("^ par ^","^string_of_expr
                               body^", "^string_of_env' env ^")"    
  | ListVal(_evs) -> "ListVal"
  | TreeVal(_t) -> "TreeVal"
  | TupleVal(evs) -> "TupleVal("^ String.concat "," (List.map
                                                           string_of_expval
                                                         evs)^")"
  | RecordVal(_fs) -> "RecordVal"
and
  string_of_env'  = function
  | EmptyEnv -> ""
  | ExtendEnv(id,v,env) -> string_of_env' env^" ("^id^","^string_of_expval v^")\n"

let string_of_env : string ea_result =
  fun env ->
  Ok ("Environment:\n"^ string_of_env' env)