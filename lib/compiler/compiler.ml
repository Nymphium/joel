open Syntax
open Common

let rec is_valuable = function
  | Data(_, args) -> List.for_all is_valuable args
  | Lamb(_, _) | True | False | Unit | Int _ | Var _ -> true
  | Add(e1, e2) | Sub(e1, e2) | Div(e1, e2) | Mul(e1, e2) | Lt(e1, e2)
    when is_valuable e1 && is_valuable e2 -> true
  | _ -> false

let rec recopt opt e = (* {{{ *)
  let e' = topopt opt e in
  match e' with
  | Unit | True | False | Var _ | Int _ -> e'
  | Lamb(x, body) -> Lamb(x, recopt opt body)
  | Add(e1, e2) -> Add(recopt opt e1, recopt opt e2)
  | Sub(e1, e2) -> Sub(recopt opt e1, recopt opt e2)
  | Mul(e1, e2) -> Mul(recopt opt e1, recopt opt e2)
  | Div(e1, e2) -> Div(recopt opt e1, recopt opt e2)
  | Lt(e1, e2) -> Lt(recopt opt e1, recopt opt e2)
  | App(e1, e2) -> App(recopt opt e1, recopt opt e2)
  | Data(kstr, args) -> Data(kstr, List.map (recopt opt) args)
  | Match(c, alt) -> Match(recopt opt c, alt |> List.map @@ fun (kstr, args, u) -> (kstr, args, recopt opt u))
  | MatchVal(c, valt) -> MatchVal(recopt opt c, valt |> List.map @@ fun (va, u) -> (va, recopt opt u))
  | Let(x, bde, body) -> Let(x, recopt opt bde, recopt opt body)
  | Letrec(x, bde, body) -> Letrec(x, recopt opt bde, recopt opt body)
  | Join(j, xvars, bde, body) ->
    Join(j, xvars, recopt opt bde, recopt opt body)
  | Joinrec(j, xvars, bde, body) ->
    Joinrec(j, xvars, recopt opt bde, recopt opt body)
  | Jump(j, xargs) -> Jump(j, (List.map (recopt opt) xargs))
  | Try(e, exc) -> Try(recopt opt e,  exc |> List.map @@ fun (ex, args, u) -> (ex, args, recopt opt u))
  | Exn(ex, args) -> Exn(ex, args |> List.map @@ recopt opt)
  | Handlers hs -> Handlers(hs |> List.map @@ fun (ex, h) -> (ex, recopt opt h))
  | Append(xs, ys) -> Append(recopt opt xs, recopt opt ys)
  | Lookup(ex, hs) -> Lookup(ex, recopt opt hs)
and topopt opt e =
  let e' = opt e in
  if compare_term e e' then
    e'
  else topopt opt e'
(* }}} *)

let joel_fullopts e = (* {{{ *)
  let open Joel_opts in
  let rec work e =
    let e' =
      match e with
      | Unit | True | False | Var _ | Int _ -> e
      | Data(kstr, args) when List.for_all is_valuable args -> e
      | Handlers _ | Append(_, _) | Lookup(_, _) -> failwith "not a joel term"
      | Lamb(x, body) -> Lamb(x, work body)
      | Add(e1, e2) ->
        let e1', e2' = work e1, work e2 in
        bin_app e1' e2' @@ fun el er -> Add(el, er)
      | Sub(e1, e2) ->
        let e1', e2' = work e1, work e2 in
        bin_app e1' e2' @@ fun el er -> Sub(el, er)
      | Mul(e1, e2) ->
        let e1', e2' = work e1, work e2 in
        bin_app e1' e2' @@ fun el er -> Mul(el, er)
      | Div(e1, e2) ->
        let e1', e2' = work e1, work e2 in
        bin_app e1' e2' @@ fun el er -> Div(el, er)
      | Lt(e1, e2) ->
        let e1', e2' = work e1, work e2 in
        bin_app e1' e2' @@ fun el er -> Lt(el, er)
      | App(fn, arg) ->
        let fn', arg' = work fn, work arg in
        if is_valuable arg' then
          App(fn', arg') |> beta
        else if is_valuable fn' then
          App(fn', arg') |> betaomega
        else
          bin_app fn' arg' @@ fun fn_ arg_ -> App(fn_, arg_)
      | Let(x, bde, body) ->
        let bde', body' = work bde, work body in
        Let(x, bde', body') |>
        begin match bde' with
          | Match(_, alt) when List.for_all (fun (_, _, u) -> is_valuable u) alt -> casefloat
          | MatchVal(_, valt) when List.for_all (fun (_, u) -> is_valuable u) valt -> casefloat
          | Match(_, _) | MatchVal(_, _) -> contifycommute
          | Let(_, _, _) | Letrec(_, _, _) -> float
          | Join(_, _, _, _) | Joinrec(_, _, _, _) -> jfloat
          | Jump(_, _) -> abort
          | _ -> fun e -> inline @@ dropvalue e
        end
      | Letrec(x, bde, body) ->
        let bde', body' = work bde, work body in
        Letrec(x, bde', body') |>
        begin match bde' with
          | Match(_, alt) when List.for_all (fun (_, _, u) -> is_valuable u) alt -> casefloat
          | MatchVal(_, valt) when List.for_all (fun (_, u) -> is_valuable u) valt -> casefloat
          | Match(_, _) | MatchVal(_, _) -> contifycommute
          | Let(_, _, _) | Letrec(_, _, _) -> float
          | Join(_, _, _, _) | Joinrec(_, _, _, _) -> jfloat
          | Jump(_, _) -> abort
          | _ -> dropvalue
        end
      | Join(j, args, bde, body) ->
        let bde', body' = work bde, work body in
        begin match Join(j, args, bde', body') |> jinline |> jdrop with
          | Join(_, _, _, _) as e' -> jfloat e'
          | u -> u
        end
      | Joinrec(j, args, bde, body) ->
        let bde', body' = work bde, work body in
        begin match Joinrec(j, args, bde', body') |> jinline |> jdrop with
          | Joinrec(_, _, _, _) as e' -> jfloat e'
          | u -> u
        end
      | Match(c, alt) ->
        let c', alt' = work c, List.map (fun (k, args, u) -> (k, args, work u)) alt in
        let e' = Match(c', alt') in
        if is_valuable c' then case e'
        else
          e' |>
          begin match c' with
            | Match(_, alt) when List.for_all (fun (_, _, u) -> is_valuable u) alt -> casefloat
            | MatchVal(_, valt) when List.for_all (fun (_, u) -> is_valuable u) valt -> casefloat
            | Match(_, _) | MatchVal(_, _) -> casefloat
            | Let(_, _, _) | Letrec(_, _, _) -> float
            | Join(_, _, _, _) | Joinrec(_, _, _, _) -> jfloat
            | Jump(_, _) -> abort
            | _ -> id
          end
      | MatchVal(c, valt) ->
        let c', valt' = work c, List.map (fun (va, u) -> (va, work u)) valt in
        let e' = MatchVal(c', valt') in
        if is_valuable c' then case e'
        else e' |>
             begin match c' with
               | Match(_, alt) when List.for_all (fun (_, _, u) -> is_valuable u) alt -> casefloat
               | MatchVal(_, valt) when List.for_all (fun (_, u) -> is_valuable u) valt -> casefloat
               | Match(_, _) | MatchVal(_, _) -> casefloat
               | Let(_, _, _) | Letrec(_, _, _) -> float
               | Join(_, _, _, _) | Joinrec(_, _, _, _) -> jfloat
               | Jump(_, _) -> abort
               | _ -> id
             end
      | Data(kstr, args) -> Data(kstr, List.map work args)
      | Exn(ex, args) -> Exn(ex, List.map work args)
      | Jump(j, args) -> Jump(j, List.map work args)
      | Try(e, exc) -> Try(work e, exc |> List.map @@ fun (ex, args, u) -> (ex, args, work u))
    in
    if compare_term e e' then e'
    else work e'
  and bin_app e1 e2 cstr =
    match e2 with
    | Match(_, alt) when List.for_all (fun (_, _, u) -> is_valuable u) alt -> casefloat @@ cstr e1 e2
    | MatchVal(_, valt) when List.for_all (fun (_, u) -> is_valuable u) valt -> casefloat @@ cstr e1 e2
    | Match(_, _) | MatchVal(_, _) -> contifycommute @@ cstr e1 e2
    | Let(_, _, _) | Letrec(_, _, _) -> float @@ cstr e1 e2
    | Join(_, _, _, _) | Joinrec(_, _, _, _) -> jfloat @@ cstr e1 e2
    | Jump(_, _) -> abort @@ cstr e1 e2
    | _ -> cstr e1 e2
  in work e
(* }}} *)

let cps_fullopts e = (* {{{ *)
  let open Cps_opts in
  let rec work e =
    let e' =
      match e with
      | Unit | True | False | Var _ | Int _ | Handlers _ -> e
      | Data(kstr, args) when List.for_all is_valuable args -> e
      | Join(_, _, _, _) | Joinrec(_, _, _, _) | Jump(_, _) | Exn(_, _) | Try(_, _) -> failwith "not a joel term"
      | Lamb(x, body) -> Lamb(x, work body)
      | Add(e1, e2) ->
        let e1', e2' = work e1, work e2 in
        bin_app e1' e2' @@ fun el er -> Add(el, er)
      | Sub(e1, e2) ->
        let e1', e2' = work e1, work e2 in
        bin_app e1' e2' @@ fun el er -> Sub(el, er)
      | Mul(e1, e2) ->
        let e1', e2' = work e1, work e2 in
        bin_app e1' e2' @@ fun el er -> Mul(el, er)
      | Div(e1, e2) ->
        let e1', e2' = work e1, work e2 in
        bin_app e1' e2' @@ fun el er -> Div(el, er)
      | Lt(e1, e2) ->
        let e1', e2' = work e1, work e2 in
        bin_app e1' e2' @@ fun el er -> Lt(el, er)
      | App(fn, arg) -> App(work fn, work arg) |> beta
      | Let(x, bde, body) ->
        let bde', body' = work bde, work body in
        Let(x, bde', body') |>
        begin match bde' with
          | Match(_, _) | MatchVal(_, _) -> case
          | _ -> fun e -> inline @@ dropvalue e
        end
      | Letrec(x, bde, body) ->
        let bde', body' = work bde, work body in
        Letrec(x, bde', body') |>
        begin match bde' with
          | Match(_, _) | MatchVal(_, _) -> case
          | _ -> fun e -> inline @@ dropvalue e
        end
      | Match(c, alt) ->
        let c', alt' = work c, List.map (fun (k, args, u) -> (k, args, work u)) alt in
        case @@ Match(c', alt')
      | MatchVal(c, valt) ->
        let c', valt' = work c, List.map (fun (va, u) -> (va, work u)) valt in
        case @@ MatchVal(c', valt')
      | Data(kstr, args) -> Data(kstr, List.map work args)
      | Lookup(_, _) -> lookup e
      | Append(_, _) -> append e
    in
    if compare_term e e' then e'
    else work e'
  and bin_app e1 e2 cstr =
    match e1, e2 with
    | Match(_, _), Match(_, _) | Match(_, _), MatchVal(_, _)
    | MatchVal(_, _), Match(_, _) | MatchVal(_, _), MatchVal(_, _) -> cstr (case e1) (case e2)
    | _ -> cstr e1 e2
  in work e
(* }}} *)

module Il = struct
  module Joel = struct
    let normalize = Joel.contify
    module Opts = struct
      include Joel_opts
      module FullStrategy = struct
        let beta = recopt beta
        let betaomega = recopt betaomega
        let inline = recopt inline
        let dropvalue = recopt dropvalue
        let jinline = recopt jinline
        let jdrop = recopt jdrop
        let case = recopt case
        let contifycommute = recopt contifycommute
        let float = recopt float
        let casefloat = recopt casefloat
        let selective_case = recopt selective_case
        let jfloat = recopt jfloat
        let abort = recopt abort
        let fullopts = joel_fullopts
      end
    end
  end

  module Cps = struct
    let normalize_with_param e k h = Cps.(App(App(cpsconv e, k), Handlers h))
    let normalize e =
      let kx = Printf.sprintf "%skinit" Cps.gflag in
      App(App(Cps.cpsconv e, (Lamb(kx, Var kx))), Handlers [])
    module Opts = struct
      include Cps_opts
      module FullStrategy = struct
        let beta = recopt beta
        (* let eta = recopt eta *)
        let inline = recopt inline
        let dropvalue = recopt dropvalue
        let case = recopt case
        let append = recopt append
        let lookup = recopt lookup
        let fullopts = cps_fullopts
      end
    end
  end
end

