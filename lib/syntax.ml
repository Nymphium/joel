type term =
  | Int of int
  | Add of term * term
  | Sub of term * term
  | Mul of term * term
  | Div of term * term
  | Lt  of term * term
  | True
  | False
  | Unit
  | Var of string
  | Lamb of string * term
  | App of term * term
  | Data of string * term list
  | Match of term * (string * string list * term) list
  | MatchVal of term * (varm * term) list
  | Let of string * term * term
  | Letrec of string * term * term
  | Exn of string * term list
  | Try of term * (string * string list * term) list
  (* only for Joel {{{ *)
  | Join of string * (string list) * term * term
  | Joinrec of string * (string list) * term * term
  | Jump of string * (term list)
  (* }}} *)
  (* only for cps {{{ *)
  | Handlers of handlers
  | Append of term * term
  | Lookup of string * term
  (* }}} *)
and varm =
  | AInt of int
  | ATrue
  | AFalse
  | AVar of string
and typ =
  | Hole (* for contification *)
  | Base
  | Bool
  | TInt
  | TUnit
  | TVar of string
  | TData of string * string list * (string * typ list) list
  | TExn
  | TDataRef of string * typ list
  | Arrow of typ * typ
  | Ttapp of typ * typ
  | Univ of string * typ
and handlers = (string * term) list

type state =
  | Term of term
  | Exndef of string * typ list
  | LetAssign of string * term
  | LetrecAssign of string * term
  | Typedef of string * string list * (string * typ list) list
and joel = state list

type value =
  | VInt of int
  | Abs of string * term
  | VData of string * value list
  | VExn of string * value list
  | VTrue
  | VFalse
  | VUnit
  | VVar of string

