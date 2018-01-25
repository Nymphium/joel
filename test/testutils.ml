open OUnit2
open Syntax
open Myparsing
open Common
open Extract

(* fair_var replaces all the variables to '_'  {{{ *)
let fair_var e =
  let idvar = "_" in
  let rec work e =
    match e with
    | Var _ -> Var idvar
    | Lamb(_, body) -> Lamb(idvar, work body)
    | App(e1, e2) -> App(work e1, work e2)
    | Add(e1, e2) -> Add(work e1, work e2)
    | Sub(e1, e2) -> Sub(work e1, work e2)
    | Mul(e1, e2) -> Mul(work e1, work e2)
    | Div(e1, e2) -> Div(work e1, work e2)
    | Lt(e1, e2) -> Lt(work e1, work e2)
    | Data(kstr, args) -> Data(kstr, List.map work args)
    | Exn(ex, args) -> Exn(ex, List.map work args)
    | Match(c, alt) ->
      let alt' = alt |> List.map @@ fun (kstr, args, u) ->
        (kstr, List. map (fun _ -> idvar) args, work u) in
      Match(work c, alt')
    | MatchVal(c, valt) ->
      let valt' = valt |> List.map @@ fun (va, u) ->
        match va with
        | AVar _ -> (AVar idvar, work u)
        | _ -> (va, work u) in
      MatchVal(work c, valt')
    | Let(_, bde, body) -> Let(idvar, work bde, work body)
    | Letrec(_, bde, body) -> Letrec(idvar, work bde, work body)
    | Join(_, vars, bde, body) ->
      Join(idvar, List.map (fun _ -> idvar) vars, work bde, work body)
    | Joinrec(_, vars, bde, body) ->
      Joinrec(idvar, List.map (fun _ -> idvar) vars, work bde, work body)
    | Jump(_, args) -> Jump(idvar, List.map work args)
    | Try(e, exc) ->
      let exc' = exc |> List.map @@ fun (ex, args, u) ->
        (ex, List.map (fun _ -> idvar) args, work u) in
      Try(work e, exc')
    | True | False | Unit | Int _ -> e
    | Handlers hs -> Handlers (hs |> List.map @@ fun (ex, h) -> (ex, work h))
    | Append(xs, ys) -> Append(work xs, work ys)
    | Lookup(ex, hs) -> Lookup(ex, work hs)
  in work e
(* }}} *)

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

let estimate_size e = e |> fair_var |> extract_of_term |> String.length

(* OCaml world {{{ *)
module OCamlKit : sig
  type t
  val parse : string -> t option
  val eval : t -> (bool * string)
  val eval_from_joel : string -> string
end = struct
  type t = Parsetree.toplevel_phrase

  let parse str =
    try
      let as_buf = Lexing.from_string str in
      Some(!Toploop.parse_toplevel_phrase as_buf)
    with
    | Syntaxerr.Error _ -> None

  let eval ast =
    try
      let ok = Toploop.execute_phrase true Format.str_formatter ast in
      let ret = Format.flush_str_formatter () in
      (ok, ret)
    with
    | Typetexp.Error(_, env, err) ->
      Typetexp.report_error env Format.str_formatter err;
      (false, Format.flush_str_formatter ())
    | Typecore.Error(_, env, err) ->
      Typecore.report_error env Format.str_formatter err;
      (false, Format.flush_str_formatter ())
    | ex -> (false,  Printexc.to_string ex)

  exception Eval_incomplete of string

  (* joel-source-string ~~> string *)
  let eval_from_joel sostr =
    let so = parsewrapper sostr in
    try
      String.concat "\n" begin
        so |> List.map @@ fun s ->
        let extracted_stat = extract_of_state s in
        match parse extracted_stat with
        | None -> raise @@ Eval_incomplete "failed to parse"
        | Some o ->
          let (ok, res) = eval o in
          if not ok then
            raise @@ Eval_incomplete res
          else res
      end
    with
    | Eval_incomplete msg ->  Printf.sprintf "abort: ``%s''" msg
    | Typetexp.Error(_, env, err) ->
      Typetexp.report_error env Format.str_formatter err;
      Printf.sprintf "abort: ``%s''" @@ Format.flush_str_formatter ()
    | ex -> Printf.sprintf "abort: ``%s''" @@ Printexc.to_string ex
end
(* }}} *)

