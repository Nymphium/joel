open OUnit2
open Syntax
open Extract
open Testutils

let is_ocamlcode str = OCamlKit.parse str <> None

let extract_test str =
  let so = parsewrapper str in
  let extracted = extract_of_joel so in
  let () = Printf.printf "~~~>\n%s\n" extracted in
  assert_bool "ill extraction" @@ is_ocamlcode extracted

let test () =
  run_test_tt_main begin
    "extract test" >::: [ (* {{{ *)
      "construction" >:: begin fun test_ctxt -> (* {{{ *)
        extract_test "Cons(a, aa)"
      end; (* }}} *)
      "function definition" >:: begin fun test_ctxt -> (* {{{ *)
        extract_test "let f = fun x -> x;;"
      end; (* }}} *)
      "exception" >:: begin fun test_ctxt -> (* {{{ *)
        extract_test "exception Failure of string;;"
      end; (* }}} *)
      "data constructor" >:: begin fun test_ctxt -> (* {{{ *)
        extract_test "Cons(1, Cons(2, Cons(5, Cons(10, Nil))))"
      end; (* }}} *)
      "special form" >:: begin fun test_ctxt -> (* {{{ *)
        extract_test "[1; 2; 5; 10]"
      end; (* }}} *)
      "recursive function and match syntax" >:: begin fun test_ctxt -> (* {{{ *)
        extract_test "
let rec map = fun f -> function
    | [] -> []
    | x :: xs -> (f x) :: (map f xs);;
let l = [true; true];;
let y = map (fun x -> string_of_bool x) l in y;;
   "
      end; (* }}} *)
      "join point" >:: begin fun test_ctxt -> (* {{{ *)
        extract_test "
join flip (f, x, y) =
  f y x
in jump flip (map, ls, x);;
      "
      end; (* }}} *)
      "join rec" >:: begin fun test_ctxt -> (* {{{ *)
        extract_test "
join rec last (ls) =
  match ls with
  | x :: y ->
    begin match y with
    | _ :: _ -> jump last (y)
    | [] -> x
    end
  | [] -> err
in
jump last ([true; true; false]);;
"
      end; (* }}} *)

      "match join" >:: begin fun test_ctxt -> (* {{{ *)
        extract_test "
match (
join j (x) = x in jump j (OK)
) with
| OK -> true;;
        "
      end; (* }}} *)
    ] (* }}} *)
  end

