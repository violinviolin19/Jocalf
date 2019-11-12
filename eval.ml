open Ast

type value = 
  | VBool of bool

type result = 
  |RValue of value

type env = unit

type state = unit

let initial_env = ()

let initial_state = ()

let string_of_value  = function 
  | VBool b -> string_of_bool b

let string_of_result = function
  | RValue v -> string_of_value v

let string_of_env env =
  failwith "Unimplemented"

let string_of_state st =
  failwith "Unimplemented"

let eval_expr (e, env, st) = 
  match e with 
  | EBool b -> (RValue (VBool b), st)

let eval_defn (d, env, st) =
  failwith "Unimplemented"

let eval_phrase (p, env, st) =
  failwith "Unimplemented"

let eval_expr_init e =
  eval_expr (e, initial_env, initial_state)