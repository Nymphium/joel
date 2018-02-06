open Syntax

(* type of (test name * test function) *)
type t = (unit -> unit)

(* this ref lists is stored the test information; test itself and optimized term  *)
let target : (string (* test name*)
              * term (* coreml term *)
              * term (* joel term *)
              * term (* cps term *)
             ) list ref = ref []

let randomseed = 10000

let create_test_list =
  fun ~init_size ~interval ~size ~maxelem ->
    Array.(to_list @@ init (size) @@ fun i -> to_list @@ init ((init_size) + i * interval) @@ fun _ -> Random.init randomseed; Random.int maxelem)

(* create test {{{ *)
let sample, batch = ref 10, ref 100
let set_sample i =
  if i < 3 then failwith "sample value must be (n >= 3)"
  else
    sample := i

let set_batch i = batch := i

let create: name: string -> (unit -> 'a) -> t =
  (* from core_bench:src/benchmark.ml {{{ *)
  let stabilize_gc () =
    let rec loop failsafe last_heap_live_words =
      if failsafe <= 0 then
        failwith "unable to stabilize the number of live words in the major heap";
      Gc.compact ();
      let stat = Gc.stat () in
      if stat.Gc.live_words <> last_heap_live_words
      then loop (failsafe - 1) stat.Gc.live_words
    in
    loop 10 0
    (* }}} *)
  in
  (* median from sorted float list *)
  let list_median flst =
    let len = List.length flst in
    if len mod 2 = 0 then
      List.nth flst ((len + 1) / 2 - 1)
    else
      let xn = List.nth flst (len / 2 - 1) in
      let xn1 = List.nth flst (len / 2) in
      (xn +. xn1) /. 2.
  in
  fun ~(name : string) test ->
  fun () ->
    let times = ref [] in
    let sample, batch = !sample, !batch in
    for s = 1 to sample do
      stabilize_gc ();
      let module Time = Core.Time in
      let t1 = Time.now () in
      for b = 1 to batch do
        ignore @@ test ()
      done;
      let t2 = Time.now () in
      times := (Time.(diff t2 t1 |> Span.to_proportional_float) /. (float_of_int batch)) :: !times;
    done;
    let result = list_median @@ List.sort (fun a b -> if a >= b then 1 else 0) !times in
    Printf.printf "  %21s %2.8f\n" name result;;
(* }}} *)

let bench (tests : t list) =
  let gc0 = Gc.get () in
  let () = Printf.printf "  benchmark %d tests\n" @@ List.length tests in
  let () = Format.print_flush () in
  (* as if core_bench -no-compaction option;
   * gc.mli says "compaction is never triggered." *)
  Gc.set { (Gc.get ()) with Gc.max_overhead = 1_000_000 };
  List.iter (fun f -> f ()) tests;
  Gc.set gc0;
  print_endline ""

let print_codesize size =(* {{{ *)
  let label = Printf.sprintf "  %-19s | %4s | %4s | %4s\n" "Name" "coreml" "Joel" "CPS" in
  let () = Printf.printf "=== CODE SIZE ===\n%s%s\n" label String.(make (length label) '-') in
  ignore @@ List.map begin fun (name, term, term_joel, term_cps) ->
    Printf.printf "%21s | %4d | %4d | %4d\n" name term term_joel term_cps
  end size
(* }}} *)

