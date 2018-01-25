open Core_bench.Std
open Common
open Syntax
open Extract
open Compiler.Il
open Testutils

(* this ref lists is stored the test information; test itself and optimized term  *)
let tests = ref []
let target : (string (* test name*)
              * term (* coreml term *)
              * term (* joel term *)
              * term (* cps term *)
             ) list ref = ref []

let create_test_list =
  Random.init 10000;
  fun ~init_size ~interval ~size ~maxelem ->
    Array.(to_list @@ init (size) @@ fun i -> to_list @@ init ((init_size) + i * interval) @@ fun _ -> Random.int maxelem)

(* utilities {{{ *)
module Joelset = struct (* {{{ *)
  let normalize = Joel.normalize
  let proc e = e |> normalize |> Joel.Opts.FullStrategy.fullopts
end (* }}} *)

module Cpsset = struct (* {{{ *)
  let normalize  = fun e -> Cps.normalize e
  let proc e = e |> normalize |> Cps.Opts.FullStrategy.fullopts
end (* }}} *)

let eval_ocaml prog =
  let open OCamlKit in
  match parse prog with
  | Some t ->
    let ok, err = eval t in
    if not ok then
      failwith @@ Printf.sprintf "failed to evaluate: ``%s''\n" err
  | None ->
    failwith @@ Printf.sprintf "failed to parse: ``%s''\n" prog

let gen_bench e () = eval_ocaml @@ extract_of_joel [Term(e)]

let termparse prog =
  match Myparsing.parse prog with
  | (Term l) :: [] -> l
  | _ ->
    failwith "term can only be parsed"

(* add_test : string(name) -> string(coreml program) -> unit {{{
 * register the program with the name as test
 * and give a string `(fun __arg__ -> %s) [...]` to the program 
 * *)
let add_test : int list list ->  string -> string -> unit =
  let open Bench.Test in
  let rec of_list = function
    | [] -> Data("Nil", [])
    | x :: xs -> Data("Cons", [Int x; of_list xs])
  in
  fun arg_lists name e ->
    let etmp e arg = Let("__arg__", arg, e) in
    let term0 = termparse e in
    let term_joel0, term_cps0 = Joelset.(proc term0),
                                Cpsset.(proc term0) in
    target := (name, term0, term_joel0, term_cps0) :: !target;

    for i = 0 to List.length arg_lists - 1  do
      let __arg__ = List.nth arg_lists i in
      let arg_size = List.length __arg__ in
      let term = etmp term0 @@ of_list __arg__ in
      let term_joel, term_cps = Joelset.(etmp term_joel0 @@ of_list __arg__),
                                Cpsset.(etmp term_cps0 @@ of_list __arg__) in
      tests := (create ~name:(Printf.sprintf "%10s (%6s) (%3d)" name "coreml" @@ arg_size) @@ gen_bench term) :: !tests;
      tests := (create ~name:(Printf.sprintf "%10s (%6s) (%3d)" name "joel" @@ arg_size) @@ gen_bench term_joel) :: !tests;
      tests := (create ~name:(Printf.sprintf "%10s (%6s) (%3d)" name "cps"  @@ arg_size) @@ gen_bench term_cps) :: !tests
    done(* }}} *)

let create_test_unit name term = Bench.Test.create ~name:name @@ gen_bench term
(* }}} *)

let make_init ss = fun () -> List.iter eval_ocaml ss

let bench init tests =
  let () = init () in
  Core.Command.run @@ Bench.make_command tests

let rec print_targets = function (* {{{ *)
  | (name, term, term_joel, term_cps) :: ts ->
    let () = Printf.printf "
=== %s ===
(coreml):
%s
------
(Joel):
%s
------
(CPS):
%s
------
"
        name
        (extract_of_term term)
        (extract_of_term term_joel)
        (extract_of_term term_cps)
    in
    print_targets ts
  | [] -> ()
(* }}} *)

let print_codesize target =(* {{{ *)
  let rec work = function
    | (name, term,term_joel, term_cps) :: ts ->
      let () = Printf.printf "%21s | %4d | %4d | %4d\n" name (estimate_size term) (estimate_size term_joel) (estimate_size term_cps) in
      work ts
    | [] -> ()
  in
  let label = Printf.sprintf "  %-19s | %4s | %4s | %4s\n" "Name" "coreml" "Joel" "CPS" in
  let () = Printf.printf "=== CODE SIZE ===\n%s%s\n" label String.(make (length label) '-') in
  work target
(* }}} *)

