open Syntax

val beta : term -> term
(* val eta  : term -> term *)
val inline : term -> term
val dropvalue : term -> term
val case : term -> term
val append : term -> term
val lookup : term -> term
