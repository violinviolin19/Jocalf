open Ast


type value = 
  | VBool of bool
  | VInt of int
  | VString of string
  | VUndefined
  | VClosure of string list * expr * env

and env = (string * value) list


type result = 
  |RValue of value
  |RException of value

let loc = -1

module Env = Map.Make(String)


type state = ((int * value) list) * ((string *int) list)

let initial_env = []

let initial_state:state = ([],[])

let string_of_value  = function 
  | VBool b -> string_of_bool b
  | VInt i -> string_of_int i 
  | VString s -> "\"" ^ String.escaped s ^ "\""
  | VUndefined -> "undefined"
  | VClosure(_,_,_) -> "<closure>"

let string_of_result = function
  | RValue v -> string_of_value v
  | RException v -> "Exception: " ^ string_of_value v

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
  match v1 with
  | VString s -> loc = loc + 1;
    ((loc, v2)::(fst st) ,alter_snd_list s (snd st))
  | _-> failwith "not supposed to happen either"



let search_ref v st = 
  match v with 
  | VString s -> search_map_list s (snd st) st
  | _ -> failwith "not a vstring for some reason"

let string_of_state st =
  failwith "Unimplemented"

let rec eval_expr (e, env, (st:state)) = 
  match e with 
  | EBool b -> (RValue (VBool b), st)
  | EInt i -> (RValue (VInt i), st)
  | EString s -> (RValue (VString s), st)
  | EUndefined -> (RValue VUndefined, st)
  | EBinop (bop, e1, e2) -> eval_bop env st bop e1 e2
  | ELet (s, e1, e2) -> eval_let_expr env st s e1 e2
  | EVar x -> eval_var env st x
  | EFun (xs, e) -> (RValue (VClosure(xs, e, env)), st)
  | EApp (e, es) -> eval_fun env st e es
  | EIf (e1, e2, e3) -> eval_if env st e1 e2 e3 
  | ERef e1 -> eval_ref e1 env st
  | EDeref e1 -> eval_deref e1 env st 
  | ERefA (e1, e2) -> eval_ref_assign e1 e2 env st
  | ESeq (e1, e2) -> eval_seq env st e1 e2
  | EAnd (e1, e2) -> eval_and env st e1 e2
  | EOr (e1, e2) -> eval_or env st e1 e2
  | EThrow e1 -> eval_throw e1 env st
  | ETry (e1, x, e2) -> eval_try e1 x e2 env st
  | ETryFinally (e1, x, e2, e3) -> eval_try_finally

and eval_bop env st bop e1 e2= match (bop, eval_expr (e1, env, st), eval_expr
                                        (e2, env, st)) with 
| BopPlus, (RValue (VInt a), st1), (RValue (VInt b), st2) -> 
  (RValue (VInt (a + b)), st)
| _ -> failwith "precondition violated"

and eval_let_expr env st s e1 e2 = 
  let v1 = fst(eval_expr(e1, env, st)) in 
  match v1 with 
  |RValue v -> eval_expr (e2, (s, v):: env, st)

and eval_var env st x =
  try (RValue (List.assoc x env), st)
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
  | RValue v -> (RValue (search_ref v st), st) (*may need to fix idk*)

and eval_ref_assign e1 e2 env st =
  match fst(eval_expr(e1, env, st)), fst(eval_expr(e2, env, st)) with
  | RValue v1, RValue v2 -> 
    if (search_ref v1 st) = VUndefined then 
      (RValue v2, st)
    else
      (RValue v2, (eval_assign v1 v2 st)) (*may need to fix, idk what eval _assign refers to anymore*)

and eval_throw e env st = 
  match fst(eval_expr(e, env, st)) with
  | RValue v -> RException v, st
  | _ -> failwith "I think this shouldn't happen" (*yeah dont know if its right*)

and eval_try e1 x e2 env st =
  match fst(eval_expr(e1, env, st)) with
  | RValue v -> failwith "This is supposed to bind e1 to x and idk how to do that yet"
  | RException v -> eval_expr(e2,env,st)  (*again no idea if this works*)

and eval_try_finally e1 x e2 e3 env st= 
  let r = eval_try e1 x e2 env st in 
  match fst(eval_expr(e3, env, st)) with
  | RException v -> RException v, st
  | RValue v -> fst r, st

and eval_seq env st e1 e2 = 
  (eval_expr (e1, env, st)) |> ignore;
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

and eval_fun env st e es = 
  match fst (eval_expr e) with 
  |RValue (VClosure(xs, e1, env)) -> eval_app env st e1 es xs
  | _ -> failwith "not gonna happen"

and eval_app env st e es xs = 
  match es, xs with 
  | [], [] -> eval_expr (e, env, st)
  | (e1 :: t1), (s1::t2) -> let r = fst(eval_expr (e1, env, st)) in begin 
      match r with 
      |RValue v -> if (List.mem_assoc s1 env) then eval_app env st e t1 t2
        else eval_app ((s1, v) :: env) st e t1 t2
      | _ -> failwith "not gonna happen"
    end 
  | _ -> failwith "not gonna happen"


let rec eval_defn (d, (env:env), st) = 
  match d with 
  | DLet (s, e) -> eval_let_defn env st s e

and eval_let_defn env st s e = 
  let v1 = fst(eval_expr (e, env, st)) in 
  match v1 with 
  |RValue v -> (v1, (s, v)::env, st)


let eval_phrase (p, env, st) =
  failwith "Unimplemented"

let eval_expr_init e =
  eval_expr (e, initial_env, initial_state)