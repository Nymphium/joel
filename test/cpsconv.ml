open OUnit2
open Common
open Syntax
open Myparsing
open Testutils
open Compiler.Il.Cps

let cps_test = comp_test ~proc: normalize

let test () =
  ()
(* run_test_tt_main begin *)
(* "cps conv test" >::: [ (* {{{ *) *)
(* "simple fun call" >:: begin fun test_ctxt -> (* {{{ *) *)
(* cps_test "f x" @@ App(App(App(Var "f", Var "x"), Lamb("k", Var "k")), Handlers []) *)
(* end; (* }}} *) *)
(* "arithmetic" >:: begin fun test_ctxt -> (* {{{ *) *)
(* cps_test "1 + 2" *)
(* ~~> "1 + 2" *)
(* end; (* }}} *) *)
(* "let" >:: begin fun test_ctxt -> (* {{{ *) *)
(* cps_test "let f = fun i -> i in f 0" *)
(* @@ Let("f", Lamb("i", Lamb("k", Lamb("h", App(Var"k", Var"i")))), App(App(App(Var"f", Int 0), Lamb("k", Var "k")), Handlers [])) *)
(* end; (* }}} *) *)
(* "try-with" >:: begin fun test_ctxt -> (* {{{ *) *)
(* cps_test " *)
   (* try *)
   (* begin match d with *)
   (* | Some(p) -> p *)
   (* | None -> raise (Ex(1)) *)
   (* end *)
   (* with *)
   (* | Ex(x) -> x + 2" *)
(* @@ Match(Var"d", [ *)
(* ("Some", ["p"], Var "p"); *)
(* ("None", [], App(Lookup("Ex", Append(Handlers [("Ex", Lamb("x", Add(Var "x", Int 2)))], Handlers[])), Int 1)) *)
(* ]) *)
(* end; (* }}} *) *)
(* "try-with2" >:: begin fun test_ctxt -> *)
(* cps_test "(fun f -> try f 0 with | Exn(x) -> x) (fun x -> raise( Exn(x)))" *)
(* @@ App(App(App(Lamb("f", Lamb("k", Lamb("h", App(App(App(Var("f"), Int(0)), Var("k")), Append(Handlers([("Exn", Lamb("x", App(Var("k"), Var("x"))))]), Var("h")))))), Lamb("x", Lamb("kx", Lamb("hx", App(Lookup("Exn", Var("hx")), Var("x")))))), *)
(* Lamb("k0", Var("k0"))), Handlers([])) *)
(* end; *)
(* ] (* }}} *) *)
(* end *)

