open Syntax
open List
open Common

module SS = Set.Make(String)

exception Not_valuable

let rec valuate = function
  | Lamb(x, body) -> Abs(x, body)
  | Data(k, args) ->
    VData(k, args |> map' valuate)
  | True -> VTrue
  | False -> VFalse
  | Unit -> VUnit
  | Int i -> VInt i
  | Var x -> VVar x
  | _ -> raise Not_valuable

let rec is_valuable = function
  | Data(_, args) -> List.for_all is_valuable args
  | Lamb(_, _) | True | False | Unit | Int _ | Var _ -> true
  (* | Add(e1, e2) | Sub(e1, e2) | Div(e1, e2) | Mul(e1, e2) | Lt(e1, e2) *)
    (* when is_valuable e1 && is_valuable e2 -> true *)
  | _ -> false

let rec termize = function
  | VInt i -> Int i
  | Abs(x, body) -> Lamb(x, body)
  | VData(k, args) -> Data(k, args |> map' termize)
  | VExn(ex, args) -> Exn(ex, args |> map' termize)
  | VTrue -> True
  | VFalse -> False
  | VUnit -> Unit
  | VVar x -> Var x

let tmpterm tl = tl Unit

let find_with_index p =
  let rec inner idx = function
    | [] -> raise Not_found
    | x :: xs ->
      if p x then (x, idx)
      else inner (succ idx) xs
  in inner 0

let replace ls n b =
  let rec inner idx = function
    | [] -> []
    | x :: xs ->
      if idx = n then b :: xs
      else x :: (inner (succ idx) xs)
  in inner 0 ls

let fv =
  let list_union = fold_left (fun a b -> SS.union a b) in
  let rec inner vars env = function
    | Var v ->
      if mem v env then vars
      else SS.add v vars
    | True | False | Unit | Int _ -> SS.empty
    | Add(e1, e2) | Sub(e1, e2) | Div(e1, e2) | Mul(e1, e2) | Lt(e1, e2) | App(e1, e2) ->
      SS.union (inner vars env e1) (inner vars env e2)
    | Lamb(x, body) -> inner vars (x :: env) body
    | Data(_, args) | Exn(_, args) -> list_union vars @@ map' (inner vars env) args
    | Match(c, alt) ->
      list_union (inner vars env c) begin
        alt |> map' @@ fun (_, args, u) ->
        let env' = fold_left (fun env' x -> x :: env') env args in
        inner vars env' u
      end
    | MatchVal(c,valt) ->
      list_union (inner vars env c) begin
        valt |> map' @@ fun (va, u) ->
        match va with
        | AVar x -> inner vars (x :: env) u
        | _ -> inner vars env u
      end
    | Let(x, bde, body) ->
      SS.union (inner vars env bde) @@ inner vars (x :: env) body
    | Letrec(x, bde, body) ->
      SS.union (inner vars (x :: env) bde) @@ inner vars (x :: env) body
    | Join(_, xvars, bde, body) ->
      let env' = fold_left (fun env' x -> x :: env') env xvars in
      SS.union (inner vars env body) @@ inner vars env' bde
    | Joinrec(_, xvars, bde, body) ->
      let env' = fold_left (fun env' x -> x :: env') env xvars in
      SS.union (inner vars env body) @@ inner vars env' bde
    | Jump(_, targs) ->
      fold_left (fun ss t -> SS.union ss (inner vars env t)) vars targs
    | Try(e, exc) ->
      list_union (inner vars env e) begin
        exc |> map' @@ fun (_, xvars, u) ->
        inner vars (xvars @ env) u
      end
    | Handlers(hs) ->
      list_union SS.empty @@ map' (fun (_, h) -> inner vars env h) hs
    | Append(xs, ys) -> SS.union (inner vars env xs) (inner vars env ys)
    | Lookup(_, e) -> inner vars env e
  in inner SS.empty []

let bv = function
  | Let(x, _, _)
  | Letrec(x, _, _) -> SS.singleton x
  | Match(_, alt) -> fold_left begin fun s (_, args, _) ->
      SS.(union s @@ of_list args)
    end SS.empty alt
  | MatchVal(_, valt) -> fold_left begin
      fun s -> function
        | (AVar x, _) ->
          SS.(add x s)
        | _ -> s
    end SS.empty valt
  | Join(_, xvars, _, _) | Joinrec(_, xvars, _, _) ->
    fold_left (fun s x -> SS.add x s) SS.empty xvars
  | _ -> SS.empty

let rec fl = function
  | Data(_, _) | Exn(_, _) | True | False | Unit | Var _ | Int _ -> SS.empty
  | Add(e1, e2) | Sub(e1, e2) | Div(e1, e2) | Mul(e1, e2) | Lt(e1, e2) ->
    SS.union (fl e1) (fl e2)
  | Lamb(_, body) -> fl body
  | App(fn, arg) -> SS.union (fl fn) (fl arg)
  | Match(c, alt) ->
    fold_left begin fun s (_, _, u) ->
      SS.union s @@ fl u
    end (fl c) alt
  | MatchVal(c, valt) ->
    fold_left begin fun s (_, u) ->
      SS.union s @@ fl u
    end (fl c) valt
  | Let(_, bde, body) | Letrec(_, bde, body) ->
    SS.union (fl bde) (fl body)
  | Join(j, _, bde, body) ->
    SS.(union (fl bde) @@ remove j @@ fl body)
  | Joinrec(j, _, bde, body) ->
    SS.(remove j @@ union (fl bde) @@ fl body)
  | Jump(j, _) ->
    SS.singleton j
  | Try(e, exc) ->
    fold_left (fun s (_, _, u) -> SS.union s @@ fl u) (fl e) exc
  | _ -> failwith "not a joel term"

let rec bl = function
  | True | False | Unit | Var _ | Int _ -> SS.empty
  | Add(e1, e2) | Sub(e1, e2) | Div(e1, e2) | Mul(e1, e2) | Lt(e1, e2) ->
    SS.union (bl e1) (bl e2)
  | Lamb(_, body) -> bl body
  | App(fn, arg) -> SS.union (bl fn) (bl arg)
  | Data(_, args) ->
    fold_left (fun s t -> SS.union s @@ bl t) SS.empty args
  | Match(c, alt) ->
    fold_left begin fun s (_, _, u) ->
      SS.union s @@ bl u
    end (bl c) alt
  | MatchVal(c, valt) ->
    fold_left begin fun s (_, u) ->
      SS.union s @@ bl u
    end (bl c) valt
  | Let(_, bde, body) | Letrec(_, bde, body) ->
    SS.union (bl bde) (bl body)
  | Join(j, _, _, body) | Joinrec(j, _, _, body) ->
    SS.(add j @@ bl body)
  | Jump(j, targs) ->
    fold_left (fun s t -> SS.union s @@ bl t) SS.empty targs
  | Try(e, exc) ->
    fold_left (fun s (_, _, u) -> SS.union s @@ bl u) (bl e) exc
  | Exn(_, trs) ->
    fold_left (fun s t -> SS.union s @@ bl t) SS.empty trs
  | _ -> failwith "not a joel term"

let separatable e us =
  (* forall u in us. (fv(u) `cap` bv(e)) `cup` (fl(u) `cap` bl(e)) = empty *)
  for_all SS.(fun u -> union (inter (fv u) (bv e)) (inter (fl u) (bl e)) |> is_empty) us

(* tail_apply L[e] k ~> L[k e] *)
let rec tail_apply tr k =
  match tr with
  | Match(c, alt) ->
    Match(c, map' (fun (cstr, args, u) -> (cstr, args, tail_apply u k)) alt)
  | MatchVal(c, valt) ->
    MatchVal(c, map' (fun (va, u) -> (va, tail_apply u k)) valt)
  | Let(x, bde, body) -> Let(x, bde, tail_apply body k)
  | Letrec(x, bde, body) -> Letrec(x, bde, tail_apply body k)
  | Join(j, vars, bde, body) -> Join(j, vars, tail_apply bde k, tail_apply body k)
  | Joinrec(j, vars, bde, body) -> Joinrec(j, vars, tail_apply bde k, tail_apply body k)
  | Try(e, exc) -> Try(e, exc |> map' @@ fun (ex, arg, u) -> (ex, arg, tail_apply u k))
  | _ -> k tr

let rec esubs e x e' =
  match e with
  | Var y when x = y -> e'
  | Lamb(y, body) when x <> y -> Lamb(y, esubs body x e')
  | App(fn, arg) -> App(esubs fn x e', esubs arg x e')
  | Let(y, bde, body) when x <> y ->
    Let(y, esubs bde x e',
        if x = y then body else esubs body x e')
  | Letrec(y, bde, body) when x <> y ->
    Letrec(y, esubs bde x e',
           if x = y then body else esubs body x e')
  | Let(_, _, _) | Letrec(_, _, _) -> e
  | Data(kstr, args) -> Data(kstr, args |> map' @@ fun t -> esubs t x e')
  | Match(c, alt) ->
    Match(esubs c x e', alt |> map' (fun (k, args, u) ->
        if mem x args then (k, args, u)
        else
          (k, args, esubs u x e')))
  | MatchVal(c, valt) ->
    MatchVal(esubs c x e', valt |> map' @@ fun (va, u) ->
             match va with
             | AVar y when x = y -> (va, u)
             | _ -> (va, esubs u x e'))
  | Join(j, vars, bde, body) ->
    Join(j, vars,
         (if mem x vars then bde
          else esubs bde x e'),
         esubs body x e')
  | Joinrec(j, vars, bde, body) ->
    Joinrec(j, vars,
            (if mem x vars then bde
             else esubs bde x e'),
            esubs body x e')
  | Jump(j, termlist) ->
    Jump(j, termlist |> map' @@ fun tr -> esubs tr x e')
  | Exn(ex, args) -> Exn(ex, args |> map' @@ fun t -> esubs t x e')
  | Try(body, exc) ->
    Try(esubs body x e', exc |> map' @@ fun (k, args, u) ->
        if mem x args then (k, args, u)
        else (k, args, esubs u x e'))
  | Add(e1, e2) -> Add(esubs e1 x e', esubs e2 x e')
  | Sub(e1, e2) -> Sub(esubs e1 x e', esubs e2 x e')
  | Mul(e1, e2) -> Mul(esubs e1 x e', esubs e2 x e')
  | Div(e1, e2) -> Div(esubs e1 x e', esubs e2 x e')
  | Lt(e1, e2) -> Lt(esubs e1 x e', esubs e2 x e')
  | True | False | Unit | Var _ | Int _ | Lamb(_, _) -> e
  | Handlers hs -> Handlers(hs |> map' @@ fun (ex, h) -> (ex, esubs h x e'))
  | Lookup(ex, hs) -> Lookup(ex, esubs hs x e')
  | Append(xs, ys) -> Append(esubs xs x e', esubs ys x e')

let ectxt ?(dig_join = true) (e, k) =
  let rec work (e, k) =
    match e with
    | u when is_valuable u -> (u, k)
    | App(fn, arg) -> app_bin fn arg k @@ fun fn' arg' -> App(fn', arg')
    | Add(e1, e2) -> app_bin e1 e2 k @@ fun e1' e2' -> Add(e1', e2')
    | Sub(e1, e2) -> app_bin e1 e2 k @@ fun e1' e2' -> Sub(e1', e2')
    | Mul(e1, e2) -> app_bin e1 e2 k @@ fun e1' e2' -> Mul(e1', e2')
    | Div(e1, e2) -> app_bin e1 e2 k @@ fun e1' e2' -> Div(e1', e2')
    | Lt(e1, e2) -> app_bin e1 e2 k @@ fun e1' e2' -> Lt(e1', e2')
    | Let(x, bde, body) ->
      if not @@ is_valuable bde then work (bde, fun hole -> k @@ Let(x, hole, body))
      else (e, k)
    | Letrec(x, bde, body) ->
      if not @@ is_valuable bde then work (bde, fun hole -> k @@ Letrec(x, hole, body))
      else (e, k)
    | Data(kstr, args) ->
      begin try
          let reva = rev args in
          let ee, idx = find_with_index is_valuable reva in
          let k' = fun hole ->
            k @@ Data(kstr, replace reva idx hole |> rev)
          in (ee, k')
        with Not_found -> (e, k)
      end
    | Exn(ex, args) ->
      begin try
          let reva = rev args in
          let ee, idx = find_with_index is_valuable reva in
          let k' = fun hole ->
            k @@ Exn(ex, replace reva idx hole |> rev)
          in (ee, k')
        with Not_found -> (e, k)
      end
    | Match(c, alt) -> work (c, fun hole -> k @@ Match(hole, alt))
    | MatchVal(c, valt) -> work (c, fun hole -> k @@ MatchVal(hole, valt))
    | Join(j, args, bde, body) ->
      if dig_join then
        work (body, fun hole -> k @@ Join(j, args, bde, hole))
      else (e, k)
    | Joinrec(j, args, bde, body) ->
      if dig_join then
        work (body, fun hole -> k @@ Joinrec(j, args, bde, hole))
      else (e, k)
    | Try(e', exc) -> work (e', fun hole -> k @@ Try(hole, exc))
    | Append(e1, e2) -> app_bin e1 e2 k @@ fun e1' e2' -> Append(e1', e2')
    | Lookup(ex, e) -> work (e, fun hole -> k @@ Lookup(ex, hole))
    | Handlers hs ->
      begin try
          let reva = rev hs in
          let (ex, ee), idx = find_with_index (fun (_, f) -> is_valuable f) reva in
          let k' = fun hole ->
            k @@ Handlers(replace reva idx (ex, hole) |> rev)
          in (ee, k')
        with Not_found -> (e, k)
      end
    | Jump(j, args) ->
      begin try
          let reva = rev args in
          let ee, idx = find_with_index is_valuable reva in
          let k' = fun hole ->
            k @@ Jump(j, replace reva idx hole |> rev)
          in (ee, k')
        with Not_found -> (e, k)
      end
    | _ -> failwith "this is a value"
  and app_bin er el k cstr =
    if is_valuable el then
      work (er, fun hole -> k @@ cstr hole el)
    else if is_valuable er then
      work (el, fun hole -> k @@ cstr er hole)
    else (cstr el er, k)
  in work (e, k)

let fctxt (e, k) =
  let app_bin er el k cstr =
    if is_valuable el then
      (er, fun hole -> k @@ cstr hole el)
    else if is_valuable er then
      (el, fun hole -> k @@ cstr er hole)
    else (cstr el er, k)
  in
  match e with
  | App(fn, arg) -> app_bin fn arg k @@ fun fn' arg' -> App(fn', arg')
  | Add(e1, e2) -> app_bin e1 e2 k @@ fun e1' e2' -> Add(e1', e2')
  | Sub(e1, e2) -> app_bin e1 e2 k @@ fun e1' e2' -> Sub(e1', e2')
  | Mul(e1, e2) -> app_bin e1 e2 k @@ fun e1' e2' -> Mul(e1', e2')
  | Div(e1, e2) -> app_bin e1 e2 k @@ fun e1' e2' -> Div(e1', e2')
  | Lt(e1, e2) -> app_bin e1 e2 k @@ fun e1' e2' -> Lt(e1', e2')
  | Let(x, bde, body) ->
    if not @@ is_valuable bde then (bde, fun hole -> k @@ Let(x, hole, body))
    else (e, k)
  | Letrec(x, bde, body) ->
    if not @@ is_valuable bde then (bde, fun hole -> k @@ Letrec(x, hole, body))
    else (e, k)
  | Data(kstr, args) ->
    begin try
        let reva = rev args in
        let ee, idx = find_with_index is_valuable reva in
        let k' = fun hole ->
          k @@ Data(kstr, replace reva idx hole |> rev)
        in (ee, k')
      with Not_found -> (e, k)
    end
  | Exn(ex, args) ->
    begin try
        let reva = rev args in
        let ee, idx = find_with_index is_valuable reva in
        let k' = fun hole ->
          k @@ Exn(ex, replace reva idx hole |> rev)
        in (ee, k')
      with Not_found -> (e, k)
    end
  | Match(c, alt) -> (c, fun hole -> k @@ Match(hole, alt))
  | MatchVal(c, valt) -> (c, fun hole -> k @@ MatchVal(hole, valt))
  | Join(j, args, bde, body) -> (body, fun hole -> k @@ Join(j, args, bde, hole))
  | Joinrec(j, args, bde, body) -> (body, fun hole -> k @@ Joinrec(j, args, bde, hole))
  | Try(e', exc) -> (e', fun hole -> k @@ Try(hole, exc))
  | Append(e1, e2) -> app_bin e1 e2 k @@ fun e1' e2' -> Append(e1', e2')
  | Lookup(ex, e) -> (e, fun hole -> k @@ Lookup(ex, hole))
  | Handlers hs ->
    begin try
        let reva = rev hs in
        let (ex, ee), idx = find_with_index (fun (_, f) -> is_valuable f) reva in
        let k' = fun hole ->
          k @@ Handlers(replace reva idx (ex, hole) |> rev)
        in (ee, k')
      with Not_found -> (e, k)
    end
  | Jump(j, args) ->
    begin try
        let reva = rev args in
        let ee, idx = find_with_index is_valuable reva in
        let k' = fun hole ->
          k @@ Jump(j, replace reva idx hole |> rev)
        in (ee, k')
      with Not_found -> (e, k)
    end
  | u when is_valuable u -> (u, k)
  | _ -> failwith "this is a value"

let compare_v_va_tag = function
  | (Int _, AInt _)
  | (True, ATrue)
  | (False, AFalse) -> true
  | _ -> false

