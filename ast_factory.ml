open Ast

let make_let_defn x e =
  failwith "Unimplemented"

let make_let_rec_defn f x e =
  failwith "Unimplemented"

let make_seq e1 e2 =
  failwith "Unimplemented"

let make_app e es =
  failwith "Unimplemented"

let make_unop uop e =
  failwith "Unimplemented"

let make_binop bop e1 e2 =
  failwith "Unimplemented"

let make_and e1 e2 =
  EAnd (e1, e2)

let make_or e1 e2 =
  EOr (e1, e2)

let make_if e1 e2 e3 =
  failwith "Unimplemented"

let make_if_partial e1 e2 =
  failwith "Unimplemented"

let make_let x e1 e2 =
  failwith "Unimplemented"

let make_let_rec f x e1 e2 =
  failwith "Unimplemented"

let make_try e1 x e2 =
  failwith "Unimplemented"

let make_try_finally e1 x e2 e3 =
  failwith "Unimplemented"

let make_throw e =
  failwith "Unimplemented"

let make_ref e =
  failwith "Unimplemented"

let make_fun xs e =
  failwith "Unimplemented"

let make_while e1 e2 =
  failwith "Unimplemented"

let make_delete_field e1 e2 =
  failwith "Unimplemented"

let make_var x =
  failwith "Unimplemented"

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
