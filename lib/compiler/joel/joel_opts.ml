open Syntax
open Computils
open List
open Common

(* utils {{{ *)
let fresh_var s = Printf.sprintf "%s%d" s @@ upcounter ()

let is_try_ctxt tl =
  let mark = "*mark*" in
  let e = tl (Var mark) in
  let rec inner = function
    | App(e1, e2) | Add(e1, e2) | Sub(e1, e2) | Div(e1, e2) | Mul(e1, e2) -> inner e1 || inner e2
    | Try(e, exc) -> contains_var mark e || exists (fun (ex, args, u) -> inner u) exc
    | Data(_, args) | Exn(_, args) -> exists inner args
    | Match(c, alt) -> inner c || exists (fun (k, args, u) -> inner u) alt
    | MatchVal(c, valt) -> inner c || exists (fun (va, u) -> inner u) valt
    | Let(_, bde, body) -> inner bde || inner body
    | Letrec(_, bde, body) -> inner bde || inner body
    | Join(_, _,  bde, body) -> inner bde || inner body
    | Joinrec(_, _,  bde, body) -> inner bde || inner body
    | Jump(_, args) -> exists inner args
    | _ -> false
  and contains_var x e =
    match e with
    | Var y when x = y -> true
    | App(e1, e2) | Add(e1, e2) | Sub(e1, e2) | Div(e1, e2) | Mul(e1, e2) -> contains_var x e1 || contains_var x e2
    | Try(e, exc) -> contains_var x e || exists (fun (_, _, u) -> contains_var x u) exc
    | Data(_, args) | Exn(_, args) | Jump(_, args) -> exists (contains_var x) args
    | Match(c, alt) -> contains_var x c || exists (fun (_, _, u) -> contains_var x u) alt
    | MatchVal(c, valt) -> contains_var x c || exists (fun (_, u) -> contains_var x u) valt
    | Let(_, bde, body) -> contains_var x bde || contains_var x body
    | Letrec(_, bde, body) -> contains_var x bde || contains_var x body
    | Join(_, _, bde, body) -> contains_var x bde || contains_var x body
    | Joinrec(_, _, bde, body) -> contains_var x bde || contains_var x body
    | _ -> false
  in inner e
(* }}} *)

let beta e =
  match e with
  | App(Lamb(x, body), arg) when is_valuable arg ->
    let x' = fresh_var x in
    Let(x', arg, esubs body x @@ Var x')
  | _ -> e

let betaomega e =
  match e with
  | App(Lamb(x, body), arg) ->
    let (e', k) = fctxt (body, id) in
    if e' = (Var x) && not @@ is_try_ctxt k then k arg
    else e
  | _ -> e

let inline e =
  match e with
  | Let(x, bde, body) ->
    if is_valuable bde then
      Let(x, bde, esubs body x bde)
    else e
  | _ -> e

let dropvalue e =
  match e with
  | Let(x, bde, body) ->
    if is_valuable bde && not @@ SS.mem x @@ fv body then
      body
    else e
  | _ -> e

let jinline e =
  match e with
  | Join(j, vars, bde, body) ->
    begin try
        (fun body' -> Join(j, vars, bde, body')) @@
        tail_apply body @@ function
        | Jump(j', trs) when j = j' ->
          let f tr x e =
            let x' = fresh_var x in
            Let(x', tr, esubs e x @@ Var x')
          in fold_right2 f trs vars bde
        | u -> u
      with Not_valuable -> e
    end
  | _ -> e

let jdrop e =
  match e with
  | Join(j, _, _, body) | Joinrec(j, _, _, body) ->
    if not @@ SS.mem j @@ fl body then
      body
    else e
  | _ -> e

let case e =
  match e with
  | Match(c, alt) when is_valuable c ->
    begin match c with
      | Data(kstr, args) ->
        let (_, args', u) = find (fun (kstr', _, _) -> kstr' = kstr) alt in
        fold_right2 (fun a x e' ->
            let x' = fresh_var x in
            Let(x', a, esubs e' x @@ Var x')) args args' u
      | Var _ -> e
      | _ -> failwith @@ Printf.sprintf "type error %s" @@ rawstring_of_term c
    end
  | MatchVal(v, valt) ->
    begin match find_opt (function (AVar x, _) when x <> "_" -> true | (va, _) -> compare_v_va_tag (v, va)) valt with
      | Some(AVar n, e') ->
        let n' = fresh_var n in
        Let(n', v, esubs e' n @@ Var n')
      | Some(_, e') -> e'
      | None -> e
    end
  | _ -> e

let contifycommute e =
  let e', k = fctxt (e, id) in
  if is_try_ctxt k then e
  else
    match e' with
    | Match(c, alt) ->
      if for_all (fun (_, _, u) -> is_valuable u) alt then
        let j = fresh_var "j" in
        let arg = fresh_var "y" in
        Join(j, [arg], k @@ Var arg, Match(c, alt |> map' @@ fun (k, arg, u) -> (k, arg, Jump(j, [u]))))
      else e
    | MatchVal(c, valt) ->
      if for_all (fun (_, u) -> is_valuable u) valt then
        let j = fresh_var "j" in
        let arg = fresh_var "y" in
        Join(j, [arg], k @@ Var arg, MatchVal(c, valt |> map' @@ fun (va, u) -> (va, Jump(j, [u]))))
      else e
    | _ -> e

let float e =
  let e', k = fctxt (e, id) in
  if is_try_ctxt k then e
  else
    let fvef = fv @@ tmpterm k in
    match e' with
    | Let(x, bde, body) ->
      if not @@ SS.mem x fvef then
        Let(x, bde, k body)
      else e
    | Letrec(x, bde, body) ->
      if not @@ SS.mem x fvef then
        Letrec(x, bde, k body)
      else e
    | _ -> e

let casefloat e =
  let e', k = fctxt (e, id) in
  if is_try_ctxt k then e
  else
    let fvef = fv @@ tmpterm k in
    match e' with
    | Match(c, alt) ->
      if for_all (fun (_, xs, _) -> SS.(is_empty @@ inter (of_list xs) fvef)) alt then
        Match(c, alt |> map' @@ fun (kstr, args, u) -> (kstr, args, k u))
      else e
    | MatchVal(c, valt) ->
      if for_all (function (AVar x, _) -> not @@ SS.mem x fvef | _ -> true) valt then
        MatchVal(c, valt |> map' @@ fun (va, u) -> (va, k u))
      else e
    | _ -> e

let selective_case e =
  let e', k = fctxt (e, id) in
  if is_try_ctxt k then e
  else
    let fvef = fv @@ tmpterm k in
    match e' with
    | Match(c, alt) ->
      if for_all (fun (_, _, u) -> not @@ is_valuable u) alt then
        (* contifycommute *)
        let j = fresh_var "j" in
        let arg = fresh_var "y" in
        Join(j, [arg], k @@ Var arg, Match(c, alt |> map' @@ fun (kstr, arg, u) -> (kstr, arg, Jump(j, [u]))))
      else
        (* casefloat *)
        begin if for_all (fun (_, xs, _) -> SS.(is_empty @@ inter (of_list xs) fvef)) alt then
            Match(c, alt |> map' @@ fun (kstr, args, u) -> (kstr, args, k u))
          else e
        end
    | MatchVal(c, valt) ->
      if for_all (fun (_, u) -> not @@ is_valuable u) valt then
        let j = fresh_var "j" in
        let arg = fresh_var "y" in
        Join(j, [arg], k @@ Var arg, MatchVal(c, valt |> map' @@ fun (va, u) -> (va, Jump(j, [u]))))
      else
        begin if for_all (function (AVar x, _) -> not @@ SS.mem x fvef | _ -> true) valt then
            MatchVal(c, valt |> map' @@ fun (va, u) -> (va, k u))
          else e
        end
    | _ -> e

let jfloat  e =
  let e', k = fctxt (e, id) in
  let ee = tmpterm k in
  match e' with
  | Join(j, vars, bde, body) ->
    if SS.((not @@ mem j (bl ee)) && is_empty @@ inter (of_list vars) @@ fv ee) then
      Join(j, vars, k bde, k body)
    else e
  | Joinrec(j, vars, bde, body) ->
    if SS.((not @@ mem j (bl ee)) && is_empty @@ inter (of_list vars) @@ fv ee) then
      Joinrec(j, vars, k bde, k body)
    else e
  | _ -> e

let abort e =
  let e', k = fctxt (e, id) in
  match e' with
  | Jump(j, es) as jmp ->
    let ee = tmpterm k in
    if not @@ SS.mem j @@ bl ee then jmp
    else e
  | _ -> e

