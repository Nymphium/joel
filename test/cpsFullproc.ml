open OUnit2
open Common
open Syntax
open Myparsing
open Testutils
open Extract
open Compiler.Il.Cps

let fullopt_test src =
  let cps = termparse_wrapper normalize src in
  let optcps = Opts.FullStrategy.fullopts cps in
  Printf.printf "%s\n~~~>\n%s\n~~OCaml~~>\n%s\n-------\n"
    src
    (string_of_term optcps)
    (extract_of_term optcps)

let test () =
  run_test_tt_main begin
    "cps full optimizatoin" >::: [ (* {{{ *)
      "simple fun call" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test "f x"
      end; (* }}} *)
      "arithmetic" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test "1 + 2"
      end; (* }}} *)
      "let" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test "let f = fun _ -> fun y -> y in f 0 1"
      end; (* }}} *)
      "try-with" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test "
try
   begin match d with
     | Some(p) -> p
     | None -> raise (Ex(1))
   end
with
| Ex(x) -> x + 2"
      end; (* }}} *)
      "match" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test "
match
  begin match e with
    | Nil -> None
    | Cons(p, ls) -> Some(p)
  end with
| None -> true
| Some(x) -> false"
      end; (* }}} *)
      "d" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test "
let rec last = fun ls ->
  match ls with
  | Cons(x, y) ->
    begin match y with
      | Cons(n_, m_) -> last y
      | Nil -> x
    end
  | Nil -> err
in
last Cons(true, Cons(true, Cons(false, Nil)))"
      end; (* }}} *)
      "any" >:: begin fun test_ctxt -> (* {{{ *)
        fullopt_test "
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
 [1; 0; 3; 2; 4]"
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
    ] (* }}} *)
  end

