open Ast

type value = 
  | VBool of bool
  | VInt of int
  | VString of string
  | VUndefined
  | VClosure

type result = 
  |RValue of value

module Env = Map.Make(String)

type env = value Env.t

type state = unit

let initial_env = Env.empty

let initial_state = ()

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
  | ESeq (e1, e2) -> eval_seq env st e1 e2
  | EAnd (e1, e2) -> eval_and env st e1 e2
  | EOr (e1, e2) -> eval_or env st e1 e2

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

and eval_seq env st e1 e2 = 
  eval_expr (e1, env, st);
  eval_expr (e2, env, st) 

and eval_and env st e1 e2= 
  let v1 = eval_expr (e1, env, st) in 
  match fst v1 with 
  |RValue (VBool false) -> v1 
  | _ -> eval_expr (e2, env, st)

and eval_or env st e1 e2= 
  let v1 = eval_expr (e1, env, st) in 
  match fst v1 with 
  |RValue (VBool true) -> v1 
  | _ -> eval_expr (e2, env, st)

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