open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds

let g_store = Store.empty_store 20 (NumVal 0)

let rec addIds fs evs =
  match fs,evs with
  | [],[] -> []
  | (id,(is_mutable,_))::t1, v::t2 -> (id,(is_mutable,v)):: addIds t1 t2
  | _,_ -> failwith "error: lists have different sizes"
      
let rec apply_proc : exp_val -> exp_val -> exp_val ea_result =
  fun f a ->
  match f with
  | ProcVal (id,body,env) ->
    return env >>+
    extend_env id a >>+
    eval_expr body
  | _ -> error "apply_proc: Not a procVal"
and
  eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) -> return @@ NumVal n
  | Var(id) -> apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return @@ NumVal (n1/n2)
  | Let(v,def,body) ->
    eval_expr def >>= 
    extend_env v >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return @@ BoolVal (n = 0)
  | IsEqual(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return  @@ BoolVal (n1=n2)
  | IsGT(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ BoolVal (n1>n2)
  | IsLT(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return  @@ BoolVal (n1<n2)
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return @@ PairVal(ev1,ev2)
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return @@ fst p 
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return @@ snd p
  | Proc(id,_,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  -> 
    eval_expr e1 >>= fun v1 ->
    eval_expr e2 >>= fun v2 ->
    apply_proc v1 v2
  | Letrec([id,par,_,_,e],target) ->
    extend_env_rec id par e >>+
    eval_expr target
  | NewRef(e) ->
    eval_expr e >>= fun ev ->
    return @@ RefVal (Store.new_ref g_store ev)
  | DeRef(e) ->
    eval_expr e >>=
    int_of_refVal >>= 
    Store.deref g_store
  | SetRef(e1,e2) ->
    eval_expr e1 >>=
    int_of_refVal >>= fun l ->
    eval_expr e2 >>= 
    Store.set_ref g_store l >>= fun _ ->
    return UnitVal
  | BeginEnd([]) ->
    return UnitVal
  | BeginEnd(es) ->
    eval_exprs es >>= fun evs ->
    return (List.hd (List.rev evs))
  | Record(fs) ->
    sequence (List.map process_field fs) >>= fun evs ->
    return (RecordVal (addIds fs evs))
  | Proj(e,id) ->
    eval_expr e >>= fields_of_recordVal >>=
    fun p -> let rec x : (string*(bool*exp_val)) list -> string -> exp_val ea_result = 
      fun p id ->
      begin
        match p with 
        | (m,(true,ex))::tl ->
          if m = id
          then int_of_refVal ex >>= fun d -> Store.deref g_store d
          else (x tl id)
        | (m,(false,ex))::tl ->
          if m = id
          then return ex
          else (x tl id)
        | _ -> failwith "invalid input"
      end 
    in (x p id)
  | SetField(e1,id,e2) ->
    eval_expr e1 >>= fields_of_recordVal >>= fun obj ->
    eval_expr e2 >>= fun v ->
      let rec x = fun t ->
        match t with 
        | (m,(true,RefVal(r)))::_ when m = id -> let _ = Store.set_ref g_store r v in return UnitVal
        | _::tl -> x tl
        | _ -> error "invalid input"
      in (x obj)
  | IsNumber(e) ->
    eval_expr e >>= fun e1 ->
    begin
      match e1 with
      | NumVal(_) -> return @@ BoolVal(true)
      | _ -> return @@ BoolVal(false)
    end
  | Unit -> return UnitVal
  | Debug(_e) ->
    string_of_env >>= fun str_env ->
    let str_store = Store.string_of_store string_of_expval g_store 
    in (print_endline (str_env^"\n"^str_store);
    error "Reached breakpoint")
  | _ -> failwith ("Not implemented: "^string_of_expr e)
and
  process_field (_id,(is_mutable,e)) =
  eval_expr e >>= fun ev ->
  if is_mutable
  then return (RefVal (Store.new_ref g_store ev))
  else return ev
and
  eval_exprs es =
  match es with
  | [] -> return []
  | h::t ->
    eval_expr h >>= fun ev ->
    eval_exprs t >>= fun evs ->
    return (ev::evs)
      
let eval_prog (AProg(_,e)) =
  eval_expr e
             
(* Interpret an expression *)
let interp (s:string) : exp_val result =
  let c = s |> parse |> eval_prog
  in run c

(* Interpret an expression read from a file with optional extension .sool *)
let interpf (s:string) : exp_val result = 
  let s = String.trim s      (* remove leading and trailing spaces *)
  in let file_name =    (* allow rec to be optional *)
       match String.index_opt s '.' with None -> s^".exr" | _ -> s
  in
  interp @@ read_file file_name


