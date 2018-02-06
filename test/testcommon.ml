open Syntax
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

let estimate_size e = e |> fair_var |> extract_of_term |> String.length

