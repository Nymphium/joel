open Syntax

val rawstring_of_type : typ -> string
val string_of_type : typ -> string
val coloredstring_of_type : typ -> string

val rawstring_of_term : term -> string
val formattedstring_of_term : term -> string
val string_of_term : term -> string

val rawstring_of_statement : state -> string
val string_of_statement : state -> string

val rawstring_of_value : value -> string
val formattedstring_of_value : value -> string
val string_of_value : value -> string

val rawstring_of_joel : joel -> string
val formattedstring_of_joel : joel -> string
val string_of_joel : joel -> string
