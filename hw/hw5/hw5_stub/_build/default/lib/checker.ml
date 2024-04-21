(*
Rohan Kallur, Daniel Kim
I pledge my honor that I have abided by the Stevens Honor System.   
*)

open ReM
open Dst
open Parser_plaf.Ast
open Parser_plaf.Parser
       
let rec chk_expr : expr -> texpr tea_result =
  fun e ->
  match e with
  | Int _n -> return IntType
  | Var id -> apply_tenv id
  | IsZero(e) ->
    chk_expr e >>= fun t ->
    if t=IntType
    then return BoolType
    else error "isZero: expected argument of type int"
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2)| Div(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    if (t1=IntType && t2=IntType)
    then return IntType
    else error "arith: arguments must be ints"
  | ITE(e1,e2,e3) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    chk_expr e3 >>= fun t3 ->
    if (t1=BoolType && t2=t3)
    then return t2
    else error "ITE: condition not boolean or types of then and else do not match"
  | Let(id,e,body) ->
    chk_expr e >>= fun t ->
    extend_tenv id t >>+
    chk_expr body
  | Proc(var,Some t1,e) ->
    extend_tenv var t1 >>+
    chk_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | Proc(_var,None,_e) ->
    error "proc: type declaration missing"
  | App(e1,e2) ->
    chk_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    chk_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Letrec([(_id,_param,None,_,_body)],_target) | Letrec([(_id,_param,_,None,_body)],_target) ->
    error "letrec: type declaration missing"
  | Letrec([(id,param,Some tParam,Some tRes,body)],target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     chk_expr body >>= fun t ->
     if t=tRes 
     then chk_expr target
     else error "LetRec: Type of recursive function does not match
declaration")
   | Pair(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    return @@ PairType(t1,t2)
  | Unpair(id1,id2,e1,e2) ->
    chk_expr e1 >>= fun t ->
    (match t with
     | PairType(t1,t2) ->
    extend_tenv id1 t1 >>+
    extend_tenv id2 t2 >>+
    chk_expr e2
     | _ -> error "unpair: expected a pair")
      
  (* EXPLICIT-REFS *)
  | BeginEnd([]) ->
    return UnitType
  | BeginEnd(es) ->
    let rec check = function
    |[] -> return UnitType
    |[e] -> chk_expr e
    |e::es -> chk_expr e >>= fun _ -> check es
  in check es
  | NewRef(e) ->
    chk_expr e >>= fun e ->
      return (RefType e)
  | DeRef(e) ->
    chk_expr e >>= (function
    |RefType t -> return t
    |_ -> error "deref: Expected a reference type")
  | SetRef(e1,e2) ->
    chk_expr e1 >>= (function
    |RefType t1 -> chk_expr e2 >>= fun t2 ->
      if t1=t2 then return UnitType
      else error "setref: Incompatible types"
    |t -> error "setref: Expected a reference type")

  (* list *)
  | EmptyList(None) ->
    error "missing type annotation"
  | EmptyList(Some t) ->
    return (ListType t)
  | Cons(h, t) ->
    chk_expr h >>= (fun x ->
      chk_expr t >>= (fun y ->
        match y with
        |ListType t -> 
          if x = t then return (ListType t)
          else error "cons: type of head and tail do not match"
        |_ -> error "cons: Expected a list"))
  | IsEmpty(e) ->
    chk_expr e >>= (fun x ->
      match x with
      |ListType _ -> return (BoolType)
      |TreeType _ -> return (BoolType)
      |_ -> error "empty?: error")
  | Hd(e) ->
    chk_expr e >>= (fun x ->
      match x with
      |ListType t -> return t
      |_ -> error "hd: Expected a list")
  | Tl(e) ->
    chk_expr e >>= (fun x ->
      match x with
      |ListType t -> return (ListType t)
      |_ -> error "tl: Expected a list")

  (* tree *)
  | EmptyTree(None) ->
    error "missing type annotation"
  | EmptyTree(Some t) ->
    return (TreeType t)
  | Node(de, le, re) ->
    chk_expr le >>= (fun ltree ->
    chk_expr re >>= (fun rtree ->
    match (ltree, rtree) with
    |(TreeType lt, TreeType rt) ->
      if lt = rt then return (TreeType lt)
      else error "node: Left and Right subtree types do not match"
    |_ -> error "node: Expected a tree"))
  | CaseT (target, emptycase, id1, id2, id3, nodecase) ->
    chk_expr target >>= (fun target ->
      match target with
      |TreeType t ->
        chk_expr emptycase >>= fun empty ->
          extend_tenv id1 t >>+
          extend_tenv id2 target >>+
          extend_tenv id3 target >>+
          chk_expr nodecase >>= fun node ->
          if empty = node then return empty
          else error "caseT: Types Don't Match"
      |_ -> error "caseT: Expected a tree")
  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug: reached breakpoint"
  | _ -> failwith "chk_expr: implement"    
and
  chk_exprs =
  fun es ->
  match es with
  | [] -> return []
  | h::tl -> chk_expr h >>= fun t ->
    chk_exprs tl >>= fun ts ->
    return (t::ts)
and
  chk_prog (AProg(_,e)) =
  chk_expr e

(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> chk_prog
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> chk_prog
  in run_teac (c >>= fun t -> return @@ string_of_texpr t)



