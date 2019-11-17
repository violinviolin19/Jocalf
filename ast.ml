(** The abstract syntax tree type. *)

(******************************************************************************
   These types (id, unop, binop) are used by the parser.  You do not want to
   change them.
 ******************************************************************************)

type id = string

type unop =
  | UopMinus
  | UopNot
  | UopTypeof
  | UopDeref

type binop =
  | BopPlus
  | BopMinus
  | BopTimes
  | BopDiv
  | BopMod
  | BopLt
  | BopLeq
  | BopGt
  | BopGeq
  | BopEq
  | BopNeq
  | BopEqStrict
  | BopNeqStrict
  | BopAssign
  | BopUpdate

(******************************************************************************
   [expr] is the type of the AST for expressions. You may implement
   this type however you wish.  Use the example interpreters seen in
   the textbook as inspiration.
 ******************************************************************************)

type expr = 
  | EBool of bool 
  | EInt of int
  | EString of string
  | EUndefined
  | EUnop  of unop * expr
  | EBinop of binop * expr * expr
  | ELet of string * expr * expr 
  | EVar of string
  | EFun of string list * expr 
  | EApp of expr * expr list
  | EIf of expr * expr * expr
  | ERef of expr 
  | ERefA of expr * expr
  | ESeq of expr * expr
  | EAnd of expr * expr 
  | EOr of expr * expr 
  (* | EWhile of expr * expr  *)
  | EThrow of expr
  | ETry of expr * string * expr
  | ETryFinally of expr * string * expr * expr

(******************************************************************************
   [defn] is the type of the AST for definitions. You may implement
   this type however you wish.  There is only one kind of
   definition---the let [rec] definition---so this type can be quite
   simple.
 ******************************************************************************)

type defn = DLet of string * expr

(******************************************************************************
   [phrase] is the type of the AST for phrases. It is used by the
   parser.  You do not want to change it.
 ******************************************************************************)

type phrase =
  | Expr of expr
  | Defn of defn
