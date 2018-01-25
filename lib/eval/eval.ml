open Syntax
open Common
open Typing

exception Raising of string * value list
exception Eval_fail of string

let rec eval t tenv env delta =
  match t with
  | True -> VTrue
  | False -> VFalse
  | Unit -> VUnit
  | Var x ->
    begin match lookup env x with
      | Some v -> v
      | _ -> raise Not_found
    end
  | Lamb (x, ty, e) -> Abs (x, ty, e)
  | App (e1, e2) ->
    begin match eval e1 tenv env delta with
      | Abs (x, ty, e) ->
        let e2' = eval e2 tenv env delta in
        eval e tenv ((x, e2') :: env) delta
      | _ -> raise @@ Eval_fail "left of `'App` must be a function"
    end
  | Tapp (Tlamb(a, t'), ty) ->
    let ct = tsubs t' a ty in
    eval ct tenv env delta
  | Tapp (Var x, ty) ->
    begin match lookup env x with
      | Some (Tabs(a, t')) ->
        let ct = tsubs t' a ty in
        eval ct tenv env delta
      | Some _ -> raise @@ Eval_fail "incompatible term"
      | None -> raise Not_found
    end
  | Tapp(_, _) -> raise @@ Eval_fail "incompatible term"
  | Tlamb (a, b) ->  Tabs (a, b)
  | Data (k, tys, es) ->
    let vs = List.map (fun e -> eval e tenv env delta) es in
    VData (k, tys, vs)
  | Match (c, alt) ->
    begin match eval c tenv env delta with
      | VData (k, tys, vs) ->
        let (_, args, u) = List.find (fun (k', _, _) -> k = k') alt in
        eval u tenv (List.fold_left2 (fun env (x, _) v -> (x, v) :: env) env args vs) delta
      |  _ -> raise @@ Eval_fail "match conditional must be a data constructor"
    end
  | Let (x, ty, bde, body) ->
    let bde' = eval bde tenv env delta in
    eval body tenv ((x, bde') :: env) delta
  | Letrec (vs, body) ->
    let rec bind_all bdes env =
      begin
        match bdes with
        | [] -> env
        | (x, _, bde)::bdes' -> (x, eval bde tenv env delta)::(bind_all bdes' env)
      end
    in eval body tenv (bind_all vs env @ env) delta
  | Join (l, xs, _, bde, e') ->
    eval e' tenv env ((l, (xs, bde)) :: delta)
  | Joinrec (ls, e') ->
    eval e' tenv env (List.fold_left (fun delta' (l, xs, _, bde) -> (l, (xs, bde)) :: delta') delta ls)
  | Jump (l, ts, es, t) ->
    begin match lookup delta l with
      | Some ((xs, bde)) ->
        let env' = List.map2 (fun x e -> (x, eval e tenv env delta)) xs es in
        eval bde tenv env' delta
      | _ -> raise Not_found
    end
  | Exn(ex, ts) -> VExn(ex, List.map (fun t -> eval t tenv env delta) ts)
  | Try(e, exc) ->
    begin try eval e tenv env delta with
      | Raising (ex, exc') ->
        (* XXX: env must have `("raise", fun (ex, exc') -> raise @@ Raising (ex, exc'))` *)
        let (_, args, u) = List.find (fun (ex', _, _) -> ex' = ex) exc in
        eval u tenv (List.fold_left2 (fun env x v -> (x, v) :: env) env args exc') delta
      | ee -> raise ee
    end

