open OUnit2
open Common
open Syntax
open Myparsing
open Testutils
open Compiler.Il.Joel

let comp_withopt opt src =
  comp_test ~proc:opt src;;

let test () =
  run_test_tt_main begin "optimizatoins test" >::: [ (* {{{ *)
      "beta"  >:: begin fun test_ctxt ->(* {{{ *)
        comp_withopt Opts.beta "(fun v -> v) x" ~~> "let v = x in v"
      end;(* }}} *)
      "betaomega" >:: begin fun test_ctxt ->(* {{{ *)
        comp_withopt Opts.betaomega "(fun v -> v + 3) (5 + 4)" ~~> "(5 + 4) + 3"
      end;(* }}} *)
      "inline" >:: begin fun test_ctxt ->(* {{{ *)
        comp_withopt Opts.inline "let x = () in f x" ~~> "let x = () in f ()"
      end;(* }}} *)
      "dropvalue" >:: begin fun test_ctxt ->(* {{{ *)
        comp_withopt Opts.dropvalue "let x = 3 in let x = p in f y" ~~> "let x = p in f y"
      end;(* }}} *)
      "jinline" >:: begin fun test_ctxt ->(* {{{ *)
        comp_withopt Opts.jinline "
join j(x) = big in
  match K with
  | A(y) -> jump j (true)
  | B -> foo
  | C -> bar"
          ~~> "
join j (x) = big in
  match K with
  | A(y) -> let x = true in big
  | B -> foo
  | C -> bar"
      end;(* }}} *)
      "float" >:: begin fun test_ctxt -> (* {{{ *)
        comp_withopt Opts.float "3 + let a = 4 in a" ~~> "let a = 4 in 3 + a"
      end; (* }}} *)
    ] end (* }}} *)
