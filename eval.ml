open Ast


type value = 
  | VBool of bool
  | VInt of int
  | VString of string
  | VUndefined
  | VClosure of string list * expr * env
  | VLocation of value

and env = (string * value) list


type result = 
  |RValue of value
  |RException of value

let loc = -1

module Env = Map.Make(String)


type state = ((int * value) list) * ((string *int) list)

let initial_env = []

let initial_state:state = ([],[])
(*i think i messed this up and will need to grab an old version*)
(** [add_to_env] will add the mapping of a variable name to its value to the
    env if not in the env already, or remap the variable name to a new given
    value, then return a new updated environment*)
let rec add_to_env v x env st accu = 
  match env with
  | [] -> (x,v)::accu
  | h :: t -> if fst h = x then (x, v)::accu @ t 
    else add_to_env v x t st (h::accu)

let string_of_value  = function 
  | VBool b -> string_of_bool b
  | VInt i -> string_of_int i 
  | VString s -> "\"" ^ String.escaped s ^ "\""
  | VUndefined -> "undefined"
  | VClosure(_,_,_) -> "<closure>"
  | VLocation s-> "<location>"

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
  match lst with(*i think i messed this up and will need to grab an old version*)
  | [] -> VUndefined
  | h :: t -> if fst h = s then search_loc_list (snd h) (fst st) 
    else search_map_list s t st

let rec alter_snd_list (v:string) lst =
  match lst with 
  | [] -> failwith "impossible :("
  | h::t -> if (fst h) = v then (v, loc)::t 
    else h::alter_snd_list v t

let eval_ref_assign_helper v1 v2 st = (*i think i messed this up and will need to grab an old version*)
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
  | EUnop (uop, e1) -> eval_uop env st uop e1
  | EBinop (bop, e1, e2) -> eval_bop env st bop e1 e2
  | ELet (s, e1, e2) -> eval_let_expr env st s e1 e2
  | EVar x -> eval_var env st x
  | EFun (xs, e) -> (RValue (VClosure(xs, e, env)), st)
  | EApp (e, es) -> eval_fun env st e es
  | EIf (e1, e2, e3) -> eval_if env st e1 e2 e3 
  | ERef e1 -> eval_ref e1 env st
  | ERefA (e1, e2) -> eval_ref_assign e1 e2 env st
  | ESeq (e1, e2) -> eval_seq env st e1 e2
  | EAnd (e1, e2) -> eval_and env st e1 e2
  | EOr (e1, e2) -> eval_or env st e1 e2
  | EWhile (e1, e2) -> eval_while env st e1 e2
  | EThrow e1 -> eval_throw e1 env st
  | ETry (e1, x, e2) -> eval_try e1 x e2 env st
  | ETryFinally (e1, x, e2, e3) -> eval_try_finally e1 x e2 e3 env st

and eval_bop env st bop e1 e2= 
  match (bop, eval_expr (e1, env, st), eval_expr
           (e2, env, st)) with 
  | BopPlus, (RValue (VInt a), st1), (RValue (VInt b), st2) -> 
    (RValue (VInt (a + b)), st)
  | _ -> failwith "bprecondition violated"

and eval_uop env st uop e1 = 
  match uop, fst(eval_expr(e1, env, st)) with
  | UopDeref, RValue (VLocation v) -> 
    (RValue (search_ref (VString (string_of_value v)) st), st)
  | UopDeref, RException v -> (RValue (VUndefined), st)
  | UopNot, RValue v -> 
    (match v with 
     | VBool b -> (RValue (VBool (not b)), st)
     | VString s -> 
       (match s with
        | "" -> (RValue (VBool true), st)
        | _-> (RValue (VBool false), st))
     | VInt i -> 
       (match i with
        | 0 -> (RValue (VBool true), st)
        | _ -> (RValue (VBool false), st))
     | VLocation l -> (RValue (VBool false), st)
     (* add cases for extern and object*)
     | VClosure (_,_,_) -> (RValue (VBool false), st)
     | VUndefined -> (RValue (VBool true), st)
     | _ -> failwith "not done with unot yet") 
  | UopTypeof, RValue v -> 
    (match v with
     | VString s -> RValue (VString "string"), st
     | VInt i -> RValue (VString "int"), st
     | VUndefined -> RValue (VString "undefined"), st
     | VBool b -> RValue (VString "bool"), st
     | VLocation l -> RValue (VString "location"), st
     | VClosure (_,_,_) -> RValue (VString "closure"), st)
  (* add typeof object later*)
  | UopMinus, RValue v ->
    (match v with
     | VUndefined -> (RValue (VUndefined), st)
     | VInt i -> (RValue (VInt (- i)), st)
     | _ -> failwith "need to implement uop minus where strings 
     can be converted to an int etc")
  | _ -> failwith "asdf"

and eval_let_expr env st s e1 e2 = 
  let v1 = fst(eval_expr(e1, env, st)) in 
  match v1 with 
  (* |RValue (VLocation v) -> eval_expr (e2, env, (fst st, (s,loc)::(snd st))) 
      not sure what to do*)
  | RValue (VLocation v) -> eval_expr (e2, env, ((loc, v)::(fst st), snd st))
  | RValue v -> eval_expr (e2, (s, v):: env, st)
  |_ -> failwith "not done yetlet_expr"

and eval_var env st x = (*need this to work for states and ref *)
  try (RValue (List.assoc x env), st)
  with Not_found -> RException(VString "Unbound variable"),st

and eval_if env st e1 e2 e3 = 
  match fst(eval_expr (e1, env, st)) with 
  | RValue(VBool true) -> eval_expr (e2, env, st)
  | RValue(VBool false) -> eval_expr (e3, env, st)
  | _ -> failwith "precondition violated"

and eval_ref e env st =
  loc = loc + 1;
  match fst(eval_expr(e, env, st)) with
  | RValue v -> (RValue (VLocation v),((loc, v)::(fst st), snd st))
  | _ -> failwith "need to change later"


and eval_ref_assign e1 e2 env st =
  match fst(eval_expr(e1, env, st)), fst(eval_expr(e2, env, st)) with
  | RValue v1, RValue v2 -> 
    if (search_ref v1 st) = VUndefined then 
      (RValue v2, st)
    else
      (RValue v2, (eval_ref_assign_helper v1 v2 st)) (*may need to fix, idk what eval _assign refers to anymore*)

and eval_throw e env st = 
  match fst(eval_expr(e, env, st)) with
  | RValue v -> RException v, st
  | _ -> failwith "I think this shouldn't happen" (*yeah dont know if its right*)

and eval_try e1 x e2 env st =
  match fst(eval_expr(e1, env, st)) with
  | RValue v -> RValue v,st
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
  match fst (eval_expr (e, env, st)) with 
  |RValue (VClosure(xs, e1, env)) -> eval_app env st e1 es xs
  | _ -> RException (VString "Application: not a function"), st

and eval_app env st e es xs = 
  match es, xs with 
  | [], [] -> eval_expr (e, env, st)
  | (e1 :: t1), (s1::t2) -> let r = fst(eval_expr (e1, env, st)) in begin 
      match r with 
      |RValue v -> if (List.mem_assoc s1 env) then eval_app env st e t1 t2
        else eval_app ((s1, v) :: env) st e t1 t2
      | _ -> failwith "bnot gonna happen"
    end 
  | _ -> RException(VString "Application: wrong number of arguments"), st

and eval_while env st e1 e2 = 
  eval_expr (EIf (e1, (ESeq (e2, EWhile (e1, e2))), EUndefined), env, st)


let rec eval_defn (d, (env:env), st) = 
  match d with 
  | DLet (s, e) -> eval_let_defn env st s e

and eval_let_defn env st s e = 
  let v1 = fst(eval_expr (e, env, st)) in 
  match v1 with 
  | RValue (VLocation v) -> v1, env, (loc, v)::(fst st), (s,loc)::(snd st)
  | RValue v -> (v1, (s, v)::env, st)
  |_ -> failwith "not done yet"


let eval_phrase (p, env, st) =
  match p with
  | Expr e -> 
    let r = eval_expr (e, env, st) in
    (fst r, env, st)
  | Defn e -> eval_defn (e, env, st)


let eval_expr_init e =
  eval_expr (e, initial_env, initial_state)