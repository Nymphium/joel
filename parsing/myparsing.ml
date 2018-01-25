open Syntax

exception ParseFailed
exception LexFailed of string

let wrap f a =
  try f Mylexer.token @@ Lexing.from_string a with
  | Myparser.Error -> raise ParseFailed
  | Mylexer.Error msg -> raise @@ LexFailed msg

let parse = wrap Myparser.parse

