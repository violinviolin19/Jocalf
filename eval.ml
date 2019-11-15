open Ast

type value = 
  | VBool of bool
  | VInt of int
  | VString of string
  | VUndefined
  | VClosure

type result = 
  |RValue of value

let loc = -1

module Env = Map.Make(String)

type env = value Env.t

type state = ((int * value) list) * ((string *int) list)

let initial_env = Env.empty

let initial_state:state = ([],[])

let string_of_value  = function 
  | VBool b -> string_of_bool b
  | VInt i -> string_of_int i 
  | VString s -> "\"" ^ String.escaped s ^ "\""
  | VUndefined -> "undefined"
  | VClosure -> "<closure>"

let string_of_result = function
  | RValue v -> string_of_value v

let string_of_env (env: env) =
  ""
let rec search_loc_list i lst =
  match lst with
  | [] -> failwith "impossible loc"
  | h :: t -> if fst h = i then snd h else search_loc_list i t


let rec search_map_list s lst st = 
  match lst with
  | [] -> VUndefined
  | h :: t -> if fst h = s then search_loc_list (snd h) (fst st) 
              else search_map_list s t st

let rec alter_snd_list (v:string) lst =
  match lst with 
  | [] -> failwith "impossible :("
  | h::t -> if (fst h) = v then (v, loc)::t 
            else h::alter_snd_list v t

let eval_assign v1 v2 st = 
  loc = loc + 1;
  ((loc, v2)::(fst st) ,alter_snd_list v1 (snd st))
  


let search_ref v st = 
  match v with 
  | s -> search_map_list s (snd st) st

let string_of_state st =
  failwith "Unimplemented"

let rec eval_expr (e, env, st) = 
  match e with 
  | EBool b -> (RValue (VBool b), st)
  | EInt i -> (RValue (VInt i), st)
  | EString s -> (RValue (VString s), st)
  | EUndefined -> (RValue VUndefined, st)
  | EBinop (bop, e1, e2) -> eval_bop env st bop e1 e2
  | ELet (s, e1, e2) -> eval_let_expr env st s e1 e2
  | EVar x -> eval_var env st x
  | EFun (_, _) -> (RValue VClosure, st)
  | EIf (e1, e2, e3) -> eval_if env st e1 e2 e3 
  | ERef e1 -> eval_ref e1 env st
  | EDeref e1 -> eval_deref e1 env st 
  | ERefA (e1, e2) -> eval_ref_assign e1 e2 env st

and eval_bop env st bop e1 e2= match (bop, eval_expr (e1, env, st), eval_expr
                                        (e2, env, st)) with 
| BopPlus, (RValue (VInt a), st1), (RValue (VInt b), st2) -> 
  (RValue (VInt (a + b)), st)
| _ -> failwith "precondition violated"

and eval_let_expr env st s e1 e2 = 
  let v1 = fst(eval_expr(e1, env, st)) in 
  match v1 with 
  |RValue v -> eval_expr (e2, Env.add s v env, st)

and eval_var env st x =
  try (RValue (Env.find x env), st)
  with Not_found -> failwith "Unbound variable"

and eval_if env st e1 e2 e3 = 
  match fst(eval_expr (e1, env, st)) with 
  | RValue(VBool true) -> eval_expr (e2, env, st)
  | RValue(VBool false) -> eval_expr (e3, env, st)
  | _ -> failwith "precondition violated"

and eval_ref e env st = 
  loc = loc + 1;
  match fst(eval_expr(e, env, st)) with
  | RValue v -> (RValue v,((loc,v)::(fst st), snd st))

and eval_deref e env st = 
  match fst(eval_expr(e, env, st)) with 
  | RValue v -> (RValue (search_ref v st), st)

and eval_ref_assign e1 e2 env st =
  match fst(eval_expr(e1, env, st)), fst(eval_expr(e2, env, st)) with
  | RValue (VString v1), RValue v2 -> 
    if (search_ref v1 st) = VUndefined then 
      (RValue v2, st)
    else
      (RValue v2, (eval_assign v1 v2 st))


let rec eval_defn (d, (env:env), st) = 
  match d with 
  | DLet (s, e) -> eval_let_defn env st s e

and eval_let_defn (env:env) st s e = 
  let v1 = fst(eval_expr (e, env, st)) in 
  match v1 with 
  |RValue v -> (v1, Env.add s v env, st)


let eval_phrase (p, env, st) =
  failwith "Unimplemented"

let eval_expr_init e =
  eval_expr (e, initial_env, initial_state)