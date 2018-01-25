open Common
open Syntax
open List
open Computils

(* helpers {{{ *)
let fresh_var s = Printf.sprintf "%s%d" s @@ upcounter ()

let dom rho =
  fold_left (fun a b -> SS.add b a) SS.empty rho 

(* dig_f (f a x) ~> (f, a, x) *)
let rec dig_f ?(trs = []) = function
  | App(fn, arg) -> dig_f ~trs:(arg :: trs) fn
  | Var f -> Some (f, trs)
  | _ -> None

let tail rho e =
  let rhov = map' fst rho in
  match dig_f e with
  | None -> e
  | Some(f, termlist) ->
    let domrho = dom rhov in
    let inters = SS.(inter (fold_left (fun s e -> union s @@ fv e) empty termlist)  @@ domrho) in
    let fallback () =
      if SS.is_empty inters then e
      else failwith "undefined behavior"
    in
    begin match assoc_opt f rho with
      | Some(xlen, Jump(label, _)) ->
        if SS.is_empty inters |> not
        || (length termlist <> xlen)
        then fallback ()
        else Jump(label, termlist)
      | _ -> fallback ()
    end

(* is_tail_fv L[e] f ~> tr in f ∈ fv(L) ? *)
let rec is_tail_fv tr f =
  match tr with
  | Match(c, alt) ->
    fv c |> SS.mem f ||
    exists (fun (_, _, u) -> is_tail_fv u f) alt
  | MatchVal(c, valt) ->
    fv c |> SS.mem f ||
    exists (fun (_, u) -> is_tail_fv u f) valt
  | Let(x, bde, body) ->
    fv bde |> SS.mem f ||
    is_tail_fv body f
  | Join(_, _, bde, body) ->
    is_tail_fv bde f || is_tail_fv body f
  | Joinrec(_, _, bde, body) ->
    is_tail_fv bde f || is_tail_fv body f
  | Var g when f = g -> true
  | _ -> false

let is_contifiable f len e =
  let rec work = function
    | Match(c, alt) ->
      not @@ SS.mem f @@ fv c &&
      alt |> for_all @@ fun (k, args, u) ->SS.(not @@ mem f @@ of_list args) && work u
    | MatchVal(c, valt) ->
      not @@ SS.mem f @@ fv c &&
      valt |> for_all (function (AVar x, u) -> x <> f  && work u | (_, u) -> work u)
    | Let(x, bde, body) when x = f -> work bde
    | Letrec(x, bde, body) when x = f -> false
    | Let(_, bde, body) | Letrec(_, bde, body) -> work bde && work body
    | Join(_, args, bde, body) | Joinrec(_, args, bde, body) ->
      if SS.(mem f @@ of_list args) then
        work body
      else
        work bde && work body
    | Try(e, exc) ->
      not @@ work e &&
      exc |> for_all (fun (ex, args, u) -> not SS.(mem f @@ of_list args) && work u)
    | App(fn, args) as e ->
      begin match dig_f e with
        | Some(g, ls) when g = f ->
          for_all (fun m -> not @@ SS.mem f @@ fv m) ls &&
          length ls = len
        | _ -> false
      end
    | u -> not @@ SS.mem f @@ fv u
  in work e
(* }}} *)

let rec contify u =
  match u with
  | Let(f, bde, body) ->
    let bde', body' = contify bde, contify body in
    let qbde, args = split_to_body_vars bde' in
    let len = length args in
    if not @@ is_contifiable f len body' then
      Let(f, bde', body')
    else
      let label = fresh_var f in
      let rho = [(f, (len, Jump(label, [])))] in
      Join(label, args, qbde, tail_apply body' (tail rho))
  | Letrec(f, bde, body) ->
    let bde', body' = contify bde, contify body in
    let qbde, args = split_to_body_vars bde' in
    let len = length args in
    if not (is_contifiable f len qbde && is_contifiable f len body') then
      Letrec(f, bde', body')
    else
      let label = fresh_var f in
      let rho = [(f, (len, Jump(label, [])))] in
      Joinrec(label, args, tail_apply qbde (tail rho), tail_apply body' (tail rho))
  | Lamb(x, body) ->
    Lamb(x, contify body)
  | Try(e, exc) ->
    let exc' = exc |> map' @@ fun (ex, args, u) -> (ex, args, contify u) in
    Try(contify e, exc')
  | Exn(ex, args) ->
    Exn(ex, map' contify args)
  | App(fn, args) -> App(contify fn, contify args)
  | Data(k, trs) -> Data(k, trs |> map' contify)
  | Match(c, alt) ->
    Match(contify c,
          alt |> map' (fun (k, args, u) -> (k, args, contify u)))
  | MatchVal(c, valt) ->
    MatchVal(contify c, valt |> map' @@ fun (va, u) -> (va, contify u))
  | Add(e1, e2) -> Add(contify e1, contify e2)
  | Sub(e1, e2) -> Sub(contify e1, contify e2)
  | Mul(e1, e2) -> Mul(contify e1, contify e2)
  | Div(e1, e2) -> Div(contify e1, contify e2)
  | Lt(e1, e2) -> Lt(contify e1, contify e2)
  | True | False | Unit | Var _ | Int _
  | Join(_, _, _, _) | Joinrec(_, _, _, _) | Jump(_, _) -> u
  | Handlers _ | Lookup(_, _) | Append(_, _) -> failwith "not a joel term"

