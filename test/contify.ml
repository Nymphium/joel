open OUnit2
open Common
open Syntax
open Myparsing
open Testutils

let contify_test src =
  comp_test ~proc:(Compiler.Il.Joel.normalize) src;;

let test () =
  run_test_tt_main begin
    "contify test" >::: [ (* {{{ *)
      "apply" >:: begin fun test_ctxt -> (* {{{ *)
        contify_test "
let f = fun x -> x in
let x = () in
  f x"
          ~~>
          "
join f (x) = x in
let x = () in
  jump f (x)"
      end; (* }}} *)

      "arithmetic" >:: begin fun test_ctxt -> (* {{{ *)
        contify_test "
let succ = fun x -> x + 1 in
succ 3"
          ~~> "
join succ (x) = x + 1 in
jump succ (3)"
      end; (* }}} *)

      "not contifiable" >:: begin fun test_ctxt -> (* {{{ *)
        contify_test "
let id = fun x -> x in
let f = fun y -> 1 + id y in
  f 5"
          ~~> 
          "
let id = fun x -> x in
join f(y) = 1 + (id y) in
  jump f (5)"
      end; (* }}} *)

      "recursive" >:: begin fun test_ctxt -> (* {{{ *)
        contify_test
          "
let rec last = fun ls ->
  match ls with
  | x :: y ->
    begin match y with
    | _ :: _ -> last y
    | [] -> x
    end
  | [] -> err in
last ([true; true; false])"
          ~~> "
join rec last (ls) = match ls with
  | x :: y ->
    begin match y with
    | _ :: _ -> jump last (y)
    | [] -> x
    end
  | [] -> err in
 jump last ([true; true; false])"
      end; (* }}} *)
      "joinrec" >:: begin fun test_ctxt -> (* {{{ *)
        contify_test "
   let rec go = fun xs ->
    match xs with
    | Cons(x, xs_) ->
      begin match p x with
        | True -> Some(x)
        | False -> go xs_
      end
    | Nil -> None
   in go xs"
          ~~> "
   join rec go (xs) =
   match xs with
   | Cons(x, xs_) ->
    begin match p x with
    | True -> Some(x)
    | False -> jump go (xs_)
    end
   | Nil -> None
   in jump go (xs)"
      end; (* }}} *)

      "any" >:: begin fun test_ctxt -> (* {{{ *)
        contify_test "
    let find = fun p xs ->
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
     any (function | 5 -> True | _ -> False) [1; 3; 45; 5]
        " ~~>
          "let find = fun p xs ->
            join rec go (xs) =
              match xs with
              | x :: xs' ->
                begin match p x with
                | True -> Some(x)
                | False -> jump go (xs')
                end
              | [] -> None
            in jump go (xs)
          in
          join any (p, xs) =
             match find p xs with
             | Some(x) -> True
             | None -> False
          in jump any ((function | 5 -> True | _ -> False), [1; 3; 45; 5])
          "
      end; (* }}} *)

      "mapold" >:: begin fun test_ctxt -> (* {{{ *)
        contify_test
          "
let map = fun f xs ->
  let rec workm = function
    | x :: xs -> (f x) :: (workm xs)
    | [] -> []
  in workm xs
in
let fold = fun f z xs ->
  let rec workf = fun z -> function
    | x :: xs -> workf (f z x) xs
    | [] -> z
  in workf z  xs
in
fold (fun x y -> x + y) 0 (map (fun x -> x * x) [1; 230; 5; 4])
"
          ~~> "
let map = fun f xs ->
  let rec workm = function
    | x :: xs -> (f x) :: (workm xs)
    | [] -> []
  in workm xs
in
join fold (f, z, xs) =
  join rec workf (z, xs) =
    match xs with
    | x :: xs' -> jump workf (f z x, xs')
    | [] -> z
  in jump workf (z, xs)
in jump fold ((fun x y -> x + y), 0, (map (fun x -> x * x) [1; 230; 5; 4]))
"
      end; (* }}} *)
      "stream fusion" >:: begin fun test_ctxt -> (* {{{ *)
        contify_test
          "
let find = fun p -> fun xs ->
    let rec go = fun xs ->
        match xs with
        | Cons(x, xs_) ->
          begin match p x with
            | true -> Some(x)
            | false -> go xs_
          end
        | Nil -> None
    in go xs
in
find (fun x -> match x with | 5 -> true | n -> false) Cons(3, Cons(5, Nil))"
          ~~> "
join find (p, xs) =
  join rec go (xs) =
    match xs with
        | Cons(x, xs_) ->
          begin match p x with
            | true -> Some(x)
            | false -> jump go (xs_)
          end
        | Nil -> None
    in jump go (xs)
in
jump find ((fun x -> match x with | 5 -> true | n -> false), Cons(3, Cons(5, Nil)))"
      end; (* }}} *)
      (* }}} *) ]
  end

