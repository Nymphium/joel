{
    open Myparser
    open Lexing
    exception Error of string
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let quot  = '\''
(* let alpha = lower | '_' *)
let under = '_'
let xlower = under | lower
let alpha = upper | lower
let xalpha = upper | xlower
let alnum = digit | xalpha | under

rule token = parse
    | digit+        { INT (int_of_string (lexeme lexbuf))  }
    | '+'           { ADD                                  }
    | '-'           { SUB                                  }
    | '/'           { DIV                                  }
    | "true"        { TRUE                                 }
    | "false"       { FALSE                                }
    | "->"          { ARROW                                }
    | "fun"         { FUN                                  }
    | "function"    { FUNCTION                             }
    | "match"       { MATCH                                }
    | "try"         { TRY                                  }
    | "with"        { WITH                                 }
    | '|'           { BRANCH                               }
    | "forall"      { FORALL                               }
    | '.'           { DOT                                  }
    | '='           { EQ                                   }
    | ','           { COMMA                                }
    | '*'           { ASTER                                }
    | ";;"          { SEMICOLS                             }
    | "::"          { COLS                                 }
    | ';'           { SEMICOL                              }
    | "let"         { LET                                  }
    | "in"          { IN                                   }
    | "rec"         { REC                                  }
    | "join"        { JOIN                                 }
    | "jump"        { JUMP                                 }
    | "begin"       { BEGIN                                }
    | "end"         { END                                  }
    | "type"        { TYPEKEY                              }
    | "of"          { OF                                   }
    | "exception"   { EXCEPTION                            }
    | "raise"       { RAISE                                }
    | '('           { LPAREN                               }
    | ')'           { RPAREN                               }
    | '<'           { LT                                   }
    | '>'           { GT                                   }
    | '['           { LSQBR                                }
    | ']'           { RSQBR                                }
    | xlower alnum* quot? { VAR (lexeme lexbuf)                  }
    | upper alnum*  { CONSTR (lexeme lexbuf)               }
    | space+        { token lexbuf                         }
    | eof           { EOF                                  }
    | _             { raise @@ Error(Printf.sprintf "At offset %d: unexpected character.\n" @@ Lexing.lexeme_start lexbuf)
                                                           }
