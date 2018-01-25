open Syntax
open Printf
open List
open Common

let concat = String.concat
let string_of_xlist delim f ls = concat delim @@ map' f ls

let opt_composit (lq, rq) c =
  if String.length c > 0 then sprintf "%s%s%s" lq c rq
  else ""

let rec decontify = function
  | Join(j, xvars, bde, body) ->
    let bde' = fold_left (fun e x -> Lamb(x, e)) (decontify bde) @@ rev xvars in
    Let(j, bde', decontify body)
  | Joinrec(j, xvars, bde, body) ->
    let bde' = fold_left (fun e x -> Lamb(x, e)) (decontify bde) @@ rev xvars in
    Letrec(j, bde', decontify body)
  | Jump(j, xargs) ->
    fold_left (fun e t -> App(e, t)) (Var j) xargs
  | e -> e

let extract_of_varm = function
  | AInt i -> string_of_int i
  | AVar x -> x
  | ATrue -> "true"
  | AFalse -> "false"

let rec extract_of_term =
  let (!.) = extract_of_term in
  let infix e1 e2 op = sprintf "(%s %s %s)" (!. e1) op (!. e2) in
  fun e -> match e with
    | Int i -> string_of_int i
    | Add(e1, e2) -> infix e1 e2 "+"
    | Sub(e1, e2) -> infix e1 e2 "-"
    | Div(e1, e2) -> infix e1 e2 "/"
    | Mul(e1, e2) -> infix e1 e2 "*"
    | Lt(e1, e2) -> infix e1 e2 "<"
    | True -> "true"
    | False -> "false"
    | Unit -> "()"
    | Var x -> x
    | Lamb(x, body) ->
      let body', xs = Common.split_to_body_vars body in
      sprintf "(fun %s -> %s)" (concat " " (x :: xs)) !.body'
    | App(fn, arg) -> sprintf "(%s) (%s)" !.fn !.arg
    | Data(kstr, args) ->
      sprintf "(%s%s)" kstr @@ opt_composit ("(", ")") @@ string_of_xlist ", " (fun i -> sprintf "(%s)" !. i) args
    | Match(c, alt) ->
      sprintf "(match (%s) with %s)" (!. c) begin
        alt |> string_of_xlist "| " @@ fun (k, args, u) ->
        !. u |>
        sprintf "%s%s -> %s" k @@ opt_composit ("(", ")") @@ concat ", " args
      end
    | MatchVal(c, valt) ->
      sprintf "(match %s with %s)"
        (!. c)
        begin
          valt |> string_of_xlist "| " @@ fun (va, u) ->
          sprintf "%s -> %s"
            (extract_of_varm va)
            (!. u)
        end
    | Let(x, bde, body) ->
      sprintf "let %s = %s in %s" x (!. bde) (!. body)
    | Letrec(x, bde, body) ->
      sprintf "let rec %s = %s in %s" x (!. bde) (!. body)
    | Exn(ex, args) -> sprintf "%s%s" ex @@ opt_composit ("(", ")") @@ string_of_xlist ", " (!.) args
    | Try(e, exc) ->
      sprintf "(try %s with %s)" (!. e) begin
        exc |> string_of_xlist "| " @@ fun (ex, args, u) ->
        sprintf "%s%s -> %s" ex (opt_composit ("(", ")") @@ concat ", " args) @@ !. u
      end
    | Join(_, _, _, _) | Joinrec(_, _, _, _) | Jump(_, _) -> !. (decontify e)
    (*
     module C = struct
       type hlist =  Cons : .. | Nil : ..
       val lookup : ...
       val (@) : ...
     end
     *)
    | Handlers hs -> fold_left (fun s (ex, h) -> sprintf "C.Cons((\"%s\", %s), %s)" ex !. h s) "C.Nil" @@ rev hs
    | Append(xs, ys) -> sprintf "(%s) C.@ (%s)" !. xs !. ys
    | Lookup(ex, hs) -> sprintf "(C.lookup \"%s\" (%s))" ex !. hs

let rec extract_of_type = function
  | Bool -> "bool"
  | TUnit -> "unit"
  | TInt -> "int"
  | TVar a -> a
  | TExn -> "exn"
  | Arrow(a, b) -> sprintf "(%s -> %s)" (extract_of_type a) (extract_of_type b)
  | TData(k, targs, _) -> sprintf "%s" k
  | TDataRef(k, targs) -> sprintf "%s" k
  (* | Univ(a, t) -> sprintf "%s" a *)
  | Ttapp(ta, tb) -> sprintf "%s %s" (extract_of_type ta) (extract_of_type tb)
  | Hole | Base | Univ(_, _) -> failwith "umm"

let extract_of_state = function
  | Term t -> sprintf "%s;;" @@ extract_of_term t
  | Exndef(ex, typs) -> sprintf "exception %s of %s;;" ex (string_of_xlist " * " extract_of_type typs)
  | LetAssign(x, tr) -> sprintf "let %s = %s;;" x @@ extract_of_term tr
  | LetrecAssign(x, tr) -> sprintf "let rec %s = %s;;" x @@ extract_of_term tr
(* | Typedef(t, tparams, kstr) -> *)
(* let tparams' = fold_left (fun s t -> sprintf "%s '%s" s t) "" tparams in *)
(* let kstr' = *)
(* kstr |> *)
(* map' @@ fun (k, ts) -> *)
(* if length ts > 0 then *)
(* let ts' = ts |> map'  (fun t -> *)
(* fold_left (fun tt a -> ttsubs tt a (TVar (sprintf "'%s" a))) t tparams *)
(* ) in *)
(* sprintf "%s of %s " k @@ string_of_xlist " * " extract_of_type ts' *)
(* else *)
(* sprintf "%s " k *)
(* in *)
(* sprintf "type%s %s = %s;;" tparams' t begin *)
(* concat "| " kstr' *)
(* end *)

let extract_of_joel = fold_left (fun l t -> sprintf "%s%s\n" l @@ extract_of_state t) ""

