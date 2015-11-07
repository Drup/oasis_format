

type flag = string

(** Test definition.
  *)
type test = string

(** Boolean expression definition.
  *)
type expr =
  | Ident of string
  | Not of expr         (** ! e *)
  | And of expr * expr  (** e1 && e2 *)
  | Or of expr * expr   (** e1 || e2 *)
  | Test of test * string (** foo(Bar) *)


type name = string

(** Abstract Syntax Tree *)
type field_op =
  | FSet of string
  | FAdd of string
  | FEval of expr


type stmt =
  | Field of name * field_op
  | IfThenElse of expr * stmt * stmt
  | Block of stmt list

type section = string

type top_stmt =
  | Section of section * name * stmt
  | Stmt of stmt
  | TopBlock of top_stmt list
