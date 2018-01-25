open Syntax

exception ParseFailed
exception LexFailed of string

val parse : string -> joel
