open Core_bench.Std
open Common
open Syntax
open Extract
open Compiler.Il
open Testutils
open Benchutils

let arg_lists = create_test_list ~init_size:1000 ~interval: 500 ~size: 1 ~maxelem: 1000

let init = make_init Benchinit.pervasives;;

let () =
  (* test case {{{ *)
  add_test arg_lists "rev" (* {{{ *)
    (* refer to lib/ocaml/list.ml *)
    "let rev = fun l ->
       let rec work = fun l1 l2 ->
         match l1 with
         | [] -> l2
         | a :: l -> work l (a :: l2)
       in work l []
     in rev __arg__";
  (* }}} *)
  add_test arg_lists "any" (* {{{ *)
    "let find = fun p xs ->
       let rec go = function
         | Cons(x, xs') ->
           begin match p x with
             | True -> Some(x)
             | False -> go xs'
           end
         | Nil -> None
       in go xs
     in
     let any = fun p xs ->
       match find p xs with
       | Some(x) -> True
       | None -> False
     in
     any (function | 5 -> True | _ -> False) __arg__";
  (* }}} *)
  add_test arg_lists "mapfold" (* {{{ *)
    "
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
fold (fun x y -> x + y) 0 (map (fun x -> x * x) __arg__)";
  (* }}} *)
  add_test arg_lists "stream" (* {{{ *)
    "
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
     fold (fun x y -> x + y) 0 (map (fun x -> x * x) (of_mylist __arg__))";
  (* }}} *)
  (* }}} *)
  let _ = List.map print_endline Benchinit.pervasives in
  let () = print_targets @@ List.rev !target in
  let () = print_endline "%%%%%" (* for processing *) in
  let () = bench init @@ List.rev !tests in
  let () = print_codesize @@ List.rev !target in
  print_endline "%%%%%"

