open OUnit2
open Syntax
open Myparsing
open Common
open Testutils

(* utils {{{ *)
let simpleparse_test prog =
  Printf.printf "%s  ...... " prog;
  try
    parse prog |> rawstring_of_joel |> print_endline
  with
  | ParseFailed -> assert_failure "invalid syntax"
  | LexFailed msg -> assert_failure msg
(* }}} *)

let test () =
  run_test_tt_main begin "parsing test" >::: [ (* {{{ *)
      "simple" >:: begin fun test_ctxt -> (* {{{ *)
        comp_test
          "let x = () in (fun y -> y) x;;"
          (Let("x", Unit, App(Lamb("y", Var("y")), Var("x"))))
      end; (* }}} *)
      "arithmetic" >:: begin fun test_ctxt -> (* {{{ *)
        comp_test
          "x + 3;;"
          (Add(Var("x"), Int(3)))
      end; (* }}} *)
      "arithmetic2" >:: begin fun test_ctxt -> (* {{{ *)
        comp_test
          "
let succ = fun x -> x + 1 in
succ 5;;"
          (Let("succ", Lamb("x", Add(Var"x", Int 1)),
               App(Var"succ", Int 5)))
      end; (* }}} *)
      "apply" >:: begin fun test_ctxt -> (* {{{ *)
        comp_test
          "f x z"
          (App(App(Var("f"), Var("x")), Var("z")))
      end; (* }}} *)
      "type apply" >:: begin fun test_ctxt -> (* {{{ *)
        comp_test "
let f = fun x -> fun y -> x in
let x = () in
let y = fun z -> z in
f x y;;"
          (Let("f",
               Lamb("x", Lamb("y", Var("x"))),
               Let("x", Unit,
                   Let("y", Lamb("z", Var("z")),
                       App(App(Var("f"), Var("x")), Var("y"))))))
      end; (* }}} *)
      "join-jump" >:: begin fun test_ctxt -> (* {{{ *)
        comp_test "join j = () in jump j"
          (Join("j", [], Unit, Jump("j", [])))
      end; (* }}} *)
      "join-jump 2" >:: begin fun test_ctxt -> (* {{{ *)
        comp_test "
join f (x, y) = x in
let x = () in
jump f(x)"
          (Join("f", ["x"; "y"], Var("x"),
                Let("x", Unit, Jump("f", [Var("x")]))))
      end; (* }}} *)
      "type definition" >:: begin fun test_ctxt -> (* {{{ *)
        let src = "
type a mylist =
  | Cons of a * mylist<a>
  | Nil;;" in
        let expected = Typedef("mylist", ["a"], [
            ("Cons", [TVar("a"); TDataRef("mylist", [TVar("a")])]);
            ("Nil", [])]) in
        try
          match parse src with
          | (Typedef(_, _, _) as tdef) :: [] ->
            assert_bool (Printf.sprintf "failed to compare") (tdef = expected)
          | _ -> assert_failure "unexpected parsed result"
        with
        | LexFailed msg -> assert_failure msg
        | ParseFailed   -> assert_failure "failed to parse"
      end; (* }}} *)
      "match" >:: begin fun test_ctxt -> (* {{{ *)
        simpleparse_test "
let rec last = fun ls ->
  match ls with
  | x :: y ->
    begin match y with
    | _ :: _ -> last y
    | [] -> x
    end
  | [] -> failwith err
in last [true; true; false]"
      end; (* }}} *)
      "match-value" >:: begin fun test_ctxt -> (* {{{ *)
        comp_test "
match e with
| true -> 1
| false -> 2"
          (MatchVal(Var "e", [
               (ATrue, Int 1);
               (AFalse, Int 2)
             ]))
      end; (* }}} *)
      "int" >:: begin fun test_ctxt -> (* {{{ *)
        simpleparse_test "
let f = fun x -> x * x in f 5
        "
      end; (* }}} *)
      "define-exception" >:: begin fun test_ctxt -> (* {{{ *)
        simpleparse_test "
exception Foo;;
exception Bar of unit;;
exception Biz of unit * base;;"
      end; (* }}} *)
      "try-with" >:: begin fun test_ctxt -> (* {{{ *)
        comp_test "
try
  e
with
| Ex(v) -> u"
          (Try(Var("e"), [("Ex", ["v"], Var("u"))]))
      end; (* }}} *)
      "list" >:: begin fun test_ctxt -> (* {{{ *)
        comp_test "f [1; 3; 5]"
        @@ App(Var "f", (Data("Cons", [Int 1; Data("Cons", [Int 3; Data("Cons", [Int 5; Data("Nil", [])])])])))
      end; (* }}} *)
    ] end (* }}} *)

