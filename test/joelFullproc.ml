open OUnit2
open Common
open Syntax
open Myparsing
open Testutils
open Extract
open Compiler.Il.Joel

(* utils {{{ *)
let fullopt_test src =
  let jo = termparse_wrapper normalize src in
  let optjo = Opts.FullStrategy.fullopts jo in
  Printf.printf "%s\n~~~>\n%s\n~~OCaml~~>\n%s\n-------\n"
    src
    (string_of_term optjo)
    (extract_of_term optjo)
(* }}} *)

let test () =
  run_test_tt_main begin
    "joel full optimization test" >::: [ (* {{{ *)
      "apply" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test "
let f = fun x -> x in
let x = () in
f x"
      end; (* }}} *)
      "type apply2" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test "
let f = fun x -> fun y -> x in
let x = () in
let y = fun z -> z in
f x y"
      end; (* }}} *)
      "casefloat" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test
          "
let e = [true; true; false; true; true] in
match (
  match e with
  | [] -> None
  | p :: ls -> Some(p)) with
| None -> true
| Some(x) -> false" 
      end; (* }}} *)
      "recursive" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test
          "
let rev = fun ls ->
  let rec work = fun l1 l2 ->
    match l1 with
    | [] -> l2
    | x :: xs -> work xs (x :: l2)
  in work ls []
in
let last = fun ls ->
  match rev ls with
  | x :: xs -> x
  | [] -> raise (Not_found)
in
last [3; 1; 5; 10; 9; 23]"
      end; (* }}} *)
      "any" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test
          "
let find = fun p xs ->
    let rec go = function
        | x :: xs' ->
          begin match p x with
            | True -> Some(x)
            | False -> go xs'
          end
        | [] -> None
    in go xs
in
let any = fun p xs ->
    match find p xs with
    | Some(x) -> True
    | None    -> False
in
any (function
    | 5 -> True 
    | _ -> False)
 __arg__"
      end; (* }}} *)
      "mapfold" >:: begin fun test_ctxt ->(* {{{ *)
        fullopt_test "
let __arg__ = [1; 3; 5; 0; 19; 22; 9] in
let map = fun f xs ->
  let rec workm = function
    | x :: xs -> (f x) :: (workm xs)
    | [] -> []
  in
  workm xs
in
let fold = fun f z xs ->
  let rec workf = fun z -> function
    | x :: xs -> workf (f z x) xs
    | [] -> z
  in workf z xs
in
fold (fun x y -> x + y) 0 (map (fun x -> x * x) __arg__)"
      end;
      (* }}} *)
      "stream" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test "
let __arg__ = [1; 3; 5; 0; 19; 22; 9] in
     let of_mylist =
     let rec list_length = function
     | Nil -> 0
     | Cons(_, xs) -> 1 + (list_length xs)
     in
     let rec list_nth = fun i -> function
     | Nil -> raise Not_found
     | Cons(x, xs) ->
     begin match i with
      | 0 -> x
      | _ -> list_nth (i - 1) xs
     end
     in
     let step = fun p ->
     match p with
     | P(i, lst) ->
     match i < list_length lst with
     | True -> Block(list_nth i lst, (P(i + 1, lst)))
     | False -> Empty
     in fun lst -> E (P(0, lst), step)
     in
     let fold = fun f z str ->
     match str with
     | E(s, step) ->
     let rec loop = fun z s ->
     match step s with
     | Empty -> z
     | Block (a, t) -> loop (f z a) t
     in loop z s
     in
     let map = fun f str ->
     match str with
     | E(s, step) ->
     let new_step = fun s ->
     match step s with
     | Empty -> Empty
     | Block(a, t) -> Block(f a, t)
     in E (s, new_step)
     in
     fold (fun x y -> x + y) 0 (map (fun x -> x * x) (of_mylist __arg__))"
      end; (* }}} *)
      (* }}} *) ]
  end

