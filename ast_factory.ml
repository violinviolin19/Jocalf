open Ast

let make_let_defn x e =
  DLet (x,e)

let make_let_rec_defn f x e =
  failwith "Unimplemented"

let make_seq e1 e2 =
  ESeq (e1, e2)

let make_app e es =
  EApp (e, es)
let make_unop uop e =
  failwith "Unimplemented"

let make_binop bop e1 e2 =
  EBinop (bop, e1, e2)

let make_and e1 e2 =
  EAnd (e1, e2)

let make_or e1 e2 =
  EOr (e1, e2)

let make_if e1 e2 e3 =
  EIf (e1, e2, e3)
let make_if_partial e1 e2 =
  EIf( e1, e2, EUndefined)

let make_let x e1 e2 =
  ELet (x, e1, e2)

let make_let_rec f x e1 e2 =
  failwith "Unimplemented"

let make_try e1 x e2 =
  ETry (e1, x, e2)

let make_try_finally e1 x e2 e3 =
  ETryFinally (e1, x, e2, e3)

let make_throw e =
  failwith "Unimplemented"

let make_ref e =
  ERef e (*need to change*)

let make_fun xs e =
  EFun (xs, e)

let make_while e1 e2 =
  (* EWhile (e1, e2) *)
  failwith "Unimplemented"

let make_delete_field e1 e2 =
  failwith "Unimplemented"

let make_var x =
  EVar x
let make_int s =
  EInt (int_of_string s)

let make_string s =
  EString s

let make_bool b =
  EBool b

let make_undefined () =
  EUndefined

let make_object fields =
  failwith "Unimplemented"

let make_get_field e1 e2 =
  failwith "Unimplemented"
