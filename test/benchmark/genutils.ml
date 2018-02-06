open Compiler.Il
open Syntax
open Extract

let termparse prog =
  match Myparsing.parse prog with
  | (Term l) :: [] -> l
  | _ ->
    failwith "term can only be parsed"

module Joelset = struct (* {{{ *)
  let normalize = Joel.normalize
  let proc s = s |> termparse |> normalize |> Joel.Opts.FullStrategy.fullopts
end (* }}} *)

module Cpsset = struct (* {{{ *)
  let normalize  = fun e -> Cps.normalize e
  let proc s = s |> termparse |> normalize |> Cps.Opts.FullStrategy.fullopts
end (* }}} *)

module Coremlset = struct
  let normalize e = e
  let proc s = s |> termparse
end
