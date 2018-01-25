open Syntax
open Computils
open List

let fresh_var s = Printf.sprintf "%s%d" s @@ Common.upcounter ()

let beta e =
  match e with
  | App(fn, arg) ->
    begin
      try
        match valuate fn with
        | Abs(x, body) ->
          let x' = fresh_var x in
          Let(x', arg, esubs body x @@ Var x')
        | _ -> raise @@ Match_failure("beta", 1, 1)
      with Match_failure _ | Not_valuable -> e
    end
  | _ -> e

(* let eta e = *)
(* match e with *)
(* | Lamb(x, App(m, Var y)) when x = y -> m *)
(* | _ -> e *)

let inline e =
  match e with
  | Let(x, bde, body) -> Let(x, bde, esubs body x bde)
  | _ -> e

let dropvalue e =
  match e with
  | Let(x, bde, body) ->
    if not @@ SS.mem x @@ fv body then
      body
    else e
  | _ -> e

let case e =
  match e with
  | Match(c, alt) when is_valuable c ->
    begin match c with
      | Data(kstr, args) ->
        let (_, args', u) = find (fun (kstr', _, _) -> kstr' = kstr) alt in
        fold_left2 (fun e' a x ->
            let x' = fresh_var x in
            Let(x', a, esubs e' x @@ Var x')) u (rev args) (rev args')
      | _  when is_valuable c -> e
      | _ -> failwith @@ Printf.sprintf "type error %s" @@ Common.rawstring_of_term e
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

let append e =
  match e with
  | Append(Handlers xs, Handlers ys) -> Handlers (xs @ ys)
  | _ -> e

let lookup e =
  match e with
  | Lookup(ex, Handlers hs) ->
    begin match find_opt (fun (ex', _) -> ex = ex') hs with
      | Some (ex', h) -> h
      | _ -> e
    end
  | _ -> e

