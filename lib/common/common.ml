open Syntax

include Printer

(* syntactic comparison {{{ *)
let compare_type ta tb = rawstring_of_type ta = rawstring_of_type tb
let compare_term ta tb = rawstring_of_term ta = rawstring_of_term tb
(* }}} *)

let id x = x

(* λx. u ~> (u, [x]) *)
let rec split_to_body_vars ?(vars = []) = function
  | Lamb(x, body) -> split_to_body_vars ~vars:(x :: vars) body
  | e -> List.(e, rev vars)

let upcounter =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

let map' f xs = List.fold_left (fun z x -> (f x) :: z) [] @@ List.rev xs
