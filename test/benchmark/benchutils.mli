open Syntax

type t = (unit -> unit)

val sample: int ref
val batch: int ref
val set_sample: int -> unit
val set_batch: int -> unit

val target: (string * term * term * term) list ref

val create_test_list:
  init_size: int ->
  interval: int ->
  size: int ->
  maxelem: int ->
  int list list

val create: name: string -> (unit -> 'a) -> t

val bench: t list -> unit

val print_codesize: (string * int * int * int) list -> unit

