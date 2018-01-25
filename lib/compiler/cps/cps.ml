open Common
open Syntax
open List
open Computils

let gflag = "__"
let flen = String.length gflag
let gensym s = Printf.sprintf "%s%s%d" gflag s @@ upcounter ()

let rec cpsconv e = (* {{{ *)
  let kx, hx = gensym "k", gensym "h" in
  match e with
  | True | False | Var _ | Int _ | Unit -> Lamb(kx, Lamb(hx, App(Var kx, e)))
  | Add(e1, e2) ->
    let ex1, ex2 = gensym "e1", gensym "e2" in
    Lamb(kx, Lamb(hx,
                  App(App(cpsconv e2,
                          Lamb(ex2, App(App(cpsconv e1,
                                            Lamb(ex1, App(Var kx, Add(Var ex1, Var ex2)))), Var hx))), Var hx)))
  | Sub(e1, e2) ->
    let ex1, ex2 = gensym "e1", gensym "e2" in
    Lamb(kx, Lamb(hx,
                  App(App(cpsconv e2,
                          Lamb(ex2, App(App(cpsconv e1,
                                            Lamb(ex1, App(Var kx, Sub(Var ex1, Var ex2)))), Var hx))), Var hx)))
  | Mul(e1, e2) ->
    let ex1, ex2 = gensym "e1", gensym "e2" in
    Lamb(kx, Lamb(hx,
                  App(App(cpsconv e2,
                          Lamb(ex2, App(App(cpsconv e1,
                                            Lamb(ex1, App(Var kx, Mul(Var ex1, Var ex2)))), Var hx))), Var hx)))
  | Div(e1, e2) ->
    let ex1, ex2 = gensym "e1", gensym "e2" in
    Lamb(kx, Lamb(hx,
                  App(App(cpsconv e2,
                          Lamb(ex2, App(App(cpsconv e1,
                                            Lamb(ex1, App(Var kx, Div(Var ex1, Var ex2)))), Var hx))), Var hx)))
  | Lt(e1, e2) ->
    let ex1, ex2 = gensym "e1", gensym "e2" in
    Lamb(kx, Lamb(hx,
                  App(App(cpsconv e2,
                          Lamb(ex2, App(App(cpsconv e1,
                                            Lamb(ex1, App(Var kx, Lt(Var ex1, Var ex2)))), Var hx))), Var hx)))
  | Lamb(x, body) ->
    let kx' = gensym "k" in
    let hx' = gensym "h" in
    Lamb(kx, Lamb(hx,
                  App(Var kx, Lamb(x, Lamb(kx', Lamb(hx', App(App(cpsconv body, Var kx'), Var hx')))))))
  | App(Var("raise"), Exn(ex, args)) ->
    let args' = args |> map @@ fun t -> (gensym "a", t) in
    Lamb(kx, Lamb(hx,
                  fold_left begin fun s (x, t) ->
                    App(App(cpsconv t, Lamb(x, s)), Var hx)
                  end (fold_left (fun s (_, x) -> App(s, x)) (Lookup(ex, Var hx)) args') @@ rev args'))
  | App(fn, arg) ->
    let a', f' = gensym "av", gensym "fv" in
    Lamb(kx, Lamb(hx,
                  App(App(cpsconv arg, Lamb(a', App(App(cpsconv fn, Lamb(f', App(App(App(Var f', Var a'), Var kx), Var hx))), Var hx))), Var hx)))
  | Let(x, bde, body) ->
    Lamb(kx, Lamb(hx,
                  if is_valuable bde then
                    Let(x, vstar bde, App(App(cpsconv body, Var kx), Var hx))
                  else
                    App(App(cpsconv bde, Lamb(x, App(App(cpsconv body, Var kx), Var hx))), Var hx)))
  | Letrec(x, bde, body) ->
    Lamb(kx, Lamb(hx,
                  if is_valuable bde then
                    Letrec(x, vstar bde, App(App(cpsconv body, Var kx), Var hx))
                  else
                    App(App(cpsconv bde, Lamb(x, App(App(cpsconv body, Var kx), Var hx))), Var hx)))
  | Data(_, _) when is_valuable e -> Lamb(kx, Lamb(hx, App(Var kx, e)))
  | Data(kst, args) ->
    let args' = args |> map @@ fun t -> (gensym "a", t) in
    Lamb(kx, Lamb(hx,
                  fold_right begin fun (x, t) s ->
                    App(App(cpsconv t, Lamb(x, s)), Var hx)
                  end args' (App(Var kx, Data(kst, args' |> map @@ fun (x, _) -> Var x)))))
  | Match(c, alt) ->
    let cv = gensym "c" in
    let k' = Lamb(cv,
                  Match(Var cv, alt |> map @@ fun (k, args, u) ->
                        (k, args, App(App(cpsconv u, Var kx), Var hx))))
    in
    Lamb(kx, Lamb(hx,
                  App(App(cpsconv c, k'), Var hx)))
  | MatchVal(c, valt) ->
    let cv = gensym "c" in
    let k' = Lamb(cv,
                  MatchVal(Var cv, valt |> map @@ fun (va, u) ->
                           (va, App(App(cpsconv u, Var kx), Var hx))))
    in
    Lamb(kx, Lamb(hx,
                  App(App(cpsconv c, k'), Var hx)))
  | Try(e, exc) ->
    let hs' = exc |> map @@ fun (ex, args, u) ->
      let seed = App(App(cpsconv u, Var kx), Var hx) in
      (ex, fold_left (fun s x -> Lamb(x, s)) seed @@ rev args)
    in
    Lamb(kx, Lamb(hx,
                  App(App(cpsconv e, Var kx), Append(Handlers hs', Var hx))))
  | Handlers _ | Lookup(_, _) | Append(_, _) -> e
  | _ -> failwith @@ Printf.sprintf "cannot convert to CPS from %s\n" @@ rawstring_of_term e
and vstar e =
  if is_valuable e then
    match e with
    | Lamb(x, body) -> 
      let kx, hx = gensym "k", gensym "h" in
      Lamb(x, Lamb(kx, Lamb(hx, App(App(cpsconv body, Var kx), Var hx))))
    | _ -> e
  else failwith "not a value"
(* }}} *)

let contains_gflag s =
  String.length s > flen &&
  (String.sub s 0 flen) = gflag

let rec squash e = (* {{{ *)
  e
(* match e with *)
(* | App(fn, arg) -> *)
(* let fn', arg' = squash fn, squash arg in *)
(* begin match fn' with *)
(* | Lamb(x, body) -> *)
(* if contains_gflag x then *)
(* esubs body x arg' |> squash *)
(* else App(Lamb(x, squash body), arg') *)
(* | _ -> App(fn', arg') *)
(* end *)
(* | Lamb(x, body) -> Lamb(x, squash body) *)
(* | Let(x, bde, body) -> *)
(* Let(x, squash bde, squash body) *)
(* | Letrec(x, bde, body) -> *)
(* Letrec(x, squash bde, squash body) *)
(* | Add(e1, e2) -> Add(squash e1, squash e2) *)
(* | Sub(e1, e2) -> Sub(squash e1, squash e2) *)
(* | Mul(e1, e2) -> Mul(squash e1, squash e2) *)
(* | Div(e1, e2) -> Div(squash e1, squash e2) *)
(* | Lt(e1, e2) -> Lt(squash e1, squash e2) *)
(* | Match(c, alt) -> Match(squash c, alt |> map @@ fun (k, args, u) -> (k, args, squash u)) *)
(* | MatchVal(c, valt) -> MatchVal(squash c, valt |> map @@ fun (va, u) -> (va, squash u)) *)
(* | Data(k, args) -> Data(k, args |> map squash) *)
(* | Handlers hs -> Handlers(hs |> map @@ fun (ex, h) -> (ex, squash h)) *)
(* | Append (xs, ys) -> Append(squash xs, squash ys) *)
(* | Lookup (ex, hs) -> Lookup(ex, squash hs) *)
(* | True | False | Var _ | Int _ | Unit -> e *)
(* | _ -> failwith "not cps term" *)
(* }}} *)

