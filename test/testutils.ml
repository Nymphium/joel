open OUnit2
open Syntax
open Myparsing
open Common
open Extract
open Testcommon

let parsewrapper prog =
  try parse prog with
  | LexFailed msg -> assert_failure msg
  | ParseFailed   -> assert_failure "failed to parse"

let termparse_wrapper k prog =
  match parsewrapper prog with
  | (Term l) :: [] -> k l
  | _ -> assert_failure "unexpected parsed result"

let fmt_term t = 
  let () = print_newline () in
  formattedstring_of_term t

let comp_test ?(proc = id) ?(formatter = fmt_term) prog ans =
  let () = Printf.printf "%s  ...... " prog in
  prog |> termparse_wrapper @@ fun h ->
  let h' = proc h in
  let () = formatter h' |> print_endline in
  compare_term (fair_var h') (fair_var ans) |> assert_bool @@
  Printf.sprintf "failed to compare ``%s'' to (%s)\nwe got (%s)"
    prog (rawstring_of_term ans) @@ rawstring_of_term h'

let (~~>) = termparse_wrapper id

(* OCaml world {{{ *)
module OCamlKit : sig
  type t
  val parse : string -> t option
  (* val eval : t -> (bool * string) *)
  (* val eval_from_joel : string -> string *)
end = struct
  type t = Parsetree.toplevel_phrase

  let parse str =
    try
      let as_buf = Lexing.from_string str in
      Some(Parse.toplevel_phrase as_buf)
    with
    | Syntaxerr.Error _ -> None

  (* let eval ast = *)
  (* try *)
  (* let ok = Toploop.execute_phrase true Format.str_formatter ast in *)
  (* let ret = Format.flush_str_formatter () in *)
  (* (ok, ret) *)
  (* with *)
  (* | Typetexp.Error(_, env, err) -> *)
  (* Typetexp.report_error env Format.str_formatter err; *)
  (* (false, Format.flush_str_formatter ()) *)
  (* | Typecore.Error(_, env, err) -> *)
  (* Typecore.report_error env Format.str_formatter err; *)
  (* (false, Format.flush_str_formatter ()) *)
  (* | ex -> (false,  Printexc.to_string ex) *)

  (* exception Eval_incomplete of string *)

  (* (* joel-source-string ~~> string *) *)
  (* let eval_from_joel sostr = *)
  (* let so = parsewrapper sostr in *)
  (* try *)
  (* String.concat "\n" begin *)
  (* so |> List.map @@ fun s -> *)
  (* let extracted_stat = extract_of_state s in *)
  (* match parse extracted_stat with *)
  (* | None -> raise @@ Eval_incomplete "failed to parse" *)
  (* | Some o -> *)
  (* let (ok, res) = eval o in *)
  (* if not ok then *)
  (* raise @@ Eval_incomplete res *)
  (* else res *)
  (* end *)
  (* with *)
  (* | Eval_incomplete msg ->  Printf.sprintf "abort: ``%s''" msg *)
  (* | Typetexp.Error(_, env, err) -> *)
  (* Typetexp.report_error env Format.str_formatter err; *)
  (* Printf.sprintf "abort: ``%s''" @@ Format.flush_str_formatter () *)
  (* | ex -> Printf.sprintf "abort: ``%s''" @@ Printexc.to_string ex *)
end
(* }}} *)

