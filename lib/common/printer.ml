open Syntax
open Printf

(* helpers {{{ *)
let concat = String.concat
let optional_brac : (string * string) -> 'a list -> ('a list -> string) -> string =
  fun tuple ls f ->
    if List.length ls > 0 then
      sprintf "%s%s%s" (fst tuple) (f ls) (snd tuple)
    else ""
let string_of_xlist delim f ls = concat delim @@ List.map f ls
let string_of_tvars ls =
  if (List.length ls) < 2 then List.nth ls 0
  else
    sprintf "(%s)" @@ String.concat ", " ls
let  id x = x
let  quot = sprintf "\"%s\""
let keyw s = sprintf "\o033[33;1m%s\o033[m" s
let typew s = sprintf "\o033[36m%s\o033[m" s
let consw s = sprintf "\o033[35m%s\o033[m" s
let funw s = sprintf "\o033[31;1m%s\o033[m" s
let cstrw s = sprintf "\o033[35;1m%s\o033[m" s
let brace b e = fun s -> sprintf "%s%s%s" (keyw b) s (keyw e)

let rawstring_of_varm = function
  | ATrue -> "ATrue"
  | AFalse  -> "AFalse"
  | AInt i -> sprintf "AInt(%d)" i
  | AVar x -> sprintf "AVar(\"%s\")" x
let string_of_varm = function
  | ATrue -> "true"
  | AFalse -> "false"
  | AInt i -> string_of_int i
  | AVar x -> x
let rec string_of_constructors ls =
  ls |> string_of_xlist "| " begin function
    | (k, []) -> k 
    | (k, tl) ->
      sprintf "%s of (%s)" k @@ string_of_xlist " * " string_of_type tl
  end
and string_of_args ls = concat ", " ls
and string_of_armdata ls =
  optional_brac ("(", ")") ls string_of_args
and string_of_vb (x, bde) =
  sprintf " %s = %s" x (string_of_term bde)
and string_of_jb (l, es, bde) =
  sprintf "%s %s = %s" l (string_of_armdata es) (string_of_term bde)
and string_of_typelist ls = string_of_xlist ", " string_of_type ls
(* }}} *)
(* exports {{{ *)
and rawstring_of_type =
  let (!.) = rawstring_of_type in
  function
  | Hole -> "Hole"
  | Base -> "Base"
  | Bool -> "Bool"
  | TInt  -> "TInt"
  | TUnit -> "TUnit"
  | TExn -> "TExn"
  | TVar v  -> sprintf "TVar(\"%s\")" v
  | Arrow(t1, t2) -> sprintf "Arrow(%s, %s)" (!. t1) (!. t2)
  | Ttapp(t1, t2) -> sprintf "Ttapp(%s, %s)" (!. t1) (!. t2)
  | Univ(x, ty) -> sprintf "Univ(\"%s\", %s)" x (!. ty)
  | TData(tg, tvars, es) ->
    let es' = es |> string_of_xlist "; " @@ fun (k, args) ->
      sprintf "(\"%s\", [%s])" k @@ string_of_xlist "; " (!.) args
    in sprintf "TData(\"%s\", [%s], [%s])" tg (string_of_xlist "; " quot tvars) es'
  | TDataRef(tg, tys) -> sprintf "TDataRef(\"%s\", [%s])" tg @@ string_of_xlist ", " (!.) tys
and string_of_type =
  let (!.) = string_of_type in
  function
  | Hole -> "<hole>"
  | Base -> "base"
  | Bool -> "bool"
  | TInt -> "int"
  | TUnit -> "unit"
  | TExn ->  "exn"
  | TVar v -> v
  | Arrow (t1, t2) -> sprintf "%s -> %s" (!. t1) (!. t2)
  | Ttapp (t1, t2) -> sprintf "%s %s" (!. t1) (!. t2)
  | Univ (x, t) -> sprintf "forall %s.%s" x (!. t)
  | TData(tyname, tvars, cstrs) ->
    sprintf "type %s %s = %s" (string_of_tvars tvars) tyname (string_of_constructors cstrs)
  | TDataRef(tag, tys) -> sprintf "%s<%s>" tag (string_of_xlist " " (!.) tys)
and coloredstring_of_type t =
  let (!.) = coloredstring_of_type in
  match t with
  | Hole -> brace "<" ">" @@ funw "hole"
  | TUnit | Base | Bool | TInt | TExn | TVar _ -> typew @@ string_of_type t
  | Arrow(t1, t2) -> sprintf "%s %s %s" (!. t1) (keyw "->") (!. t2)
  | Ttapp(t1, t2) -> sprintf "%s %s" (!. t1) (!. t2)
  | Univ(x, t') ->
    sprintf "%s %s%s%s"
      (keyw "forall")
      (typew x) (keyw ".") @@ !. t'
  | TData(tyname, tvars, cstrs) ->
    sprintf "%s %s %s %s %s"
      (keyw "type")
      (string_of_tvars tvars)
      tyname
      (typew "=")
      (string_of_constructors cstrs)
  | TDataRef(tg, tys) -> sprintf "%s%s" (typew tg) @@ brace "<" ">" @@ string_of_xlist " " (!.) tys
and rawstring_of_term =
  let (!.) = rawstring_of_term in
  function
  | True -> "True"
  | False -> "False"
  | Unit -> "Unit"
  | Var v -> sprintf "Var(\"%s\")" v
  | Int i -> sprintf "Int(%d)" i
  | Add(i1, i2) -> sprintf "Add(%s, %s)" (!. i1) (!. i2)
  | Sub(i1, i2) -> sprintf "Sub(%s, %s)" (!. i1) (!. i2)
  | Mul(i1, i2) -> sprintf "Mul(%s, %s)" (!. i1) (!. i2)
  | Div(i1, i2) -> sprintf "Div(%s, %s)" (!. i1) (!. i2)
  | Lt(i1, i2) -> sprintf "Lt(%s, %s)" (!. i1) (!. i2)
  | Lamb(x, tr) -> sprintf "Lamb(\"%s\", %s)" x (!. tr)
  | App(fn, arg) -> sprintf "App(%s, %s)" (!. fn) (!. arg)
  | Data(k, es) ->
    sprintf "Data(\"%s\", [%s])"
      k
      (string_of_xlist "; " (!.) es)
  | Match(c, alt) ->
    sprintf "Match(%s, [%s])" (!. c) begin
      alt |> string_of_xlist "; " @@ fun (k, xs, u) ->
      let xs' = string_of_xlist "; " quot xs
      in sprintf "(\"%s\", [%s], %s)" k xs' (!. u)
    end
  | MatchVal(c, valt) ->
    sprintf "MatchVal(%s, [%s])" (!. c) begin
      valt |> string_of_xlist "; " @@ fun (va, u) ->
      sprintf "(%s, %s)" (rawstring_of_varm va) (rawstring_of_term u)
    end
  | Let(x, bde, body) -> sprintf "Let(\"%s\", %s, %s)" x (!. bde) @@ !. body
  | Letrec(x, bde, body) -> sprintf "Letrec(\"%s\", %s, %s)" x (!. bde) @@ !. body
  | Join(l, es, bde, body) ->
    let es' = string_of_xlist "; " quot es in
    sprintf "Join(\"%s\", [%s], %s, %s)"
      l
      es'
      (!. bde)
      (!. body)
  | Joinrec(l, es, bde, body) ->
    let es' = string_of_xlist "; " quot es in
    sprintf "Joinrec(\"%s\", [%s], %s, %s)"
      l
      es'
      (!. bde)
      (!. body)
  | Jump(l, trs) ->
    sprintf "Jump(\"%s\", [%s])"
      l
      (string_of_xlist "; " (!.) trs)
  | Try(e, exc) ->
    sprintf "Try(%s, [%s])" (!. e) (string_of_xlist "; " (fun (k, args, u) ->
        sprintf "(\"%s\", [%s], %s)" k (string_of_xlist "; " quot args) (!. u)
      ) exc)
  | Exn(k, args) -> sprintf "Exn(\"%s\", [%s])" k @@ string_of_xlist "; " (fun x -> quot @@ !. x) args
  | Handlers hs -> sprintf "Handlers([%s])" @@ string_of_xlist "; " (fun (ex, hd) -> sprintf "(%s, %s)" (quot ex) (!. hd)) hs
  | Append(xs, ys) -> sprintf "Append(%s, %s)" (!. xs) (!. ys)
  | Lookup(ex, h) -> sprintf "Lookup(%s, %s)" (quot ex) (!. h)
and formatted_of_varm e =
  match e with
  | AInt i -> string_of_int i
  | AVar v -> v
  | ATrue | AFalse -> consw @@ string_of_varm e
and formatted_of_term_idtx idtx e =
  let (!.) = formatted_of_term_idtx in
  let makeidt idtx = String.make (idtx * 2) ' ' in
  let idt = makeidt idtx in
  let infix e1 e2 op = brace "(" ")" @@ sprintf "%s %s %s" (!. idtx e1) (keyw op) (!. idtx e2) in
  match e with
  | Var v -> v
  | Int i -> string_of_int i
  | Add(e1, e2) -> infix e1 e2 "+"
  | Sub(e1, e2) -> infix e1 e2 "-"
  | Mul(e1, e2) -> infix e1 e2 "*"
  | Div(e1, e2) -> infix e1 e2 "/"
  | Lt(e1, e2) -> infix e1 e2 "<"
  | True | False | Unit -> consw @@ string_of_term e
  | Lamb(x, tr) -> brace "(" ")" @@ sprintf "%s %s %s\n%s%s"
      (funw "fun") x (keyw "->")
      (makeidt @@ idtx + 1) (!. (idtx + 1) tr)
  | App (fn, arg) -> sprintf "%s %s" (!. idtx fn) (!. idtx arg)
  | Data (k, es) ->
    brace "(" ")" @@
    sprintf "%s%s"
      (cstrw k) @@
    optional_brac (keyw"(", keyw")") es @@ string_of_xlist (keyw", ") @@ fun e -> brace "(" ")" @@ !. (idtx + 1) e
  | Match (c, alt) ->
    brace "(" ")" @@ sprintf "%s %s %s\n%s"
      (keyw "match")
      (!. idtx c)
      (keyw "with") begin
      alt |> string_of_xlist "\n" @@ fun (k, args, e) ->
      sprintf "%s%s %s%s %s\n%s%s"
        idt (keyw "|") (cstrw k)
        (optional_brac (keyw"(", keyw")") args @@ string_of_xlist (keyw ", ") id)
        (keyw "->") (idt ^ "  ") (!. (idtx + 1) e)
    end
  | MatchVal(c, valt) ->
    brace "(" ")" @@ sprintf "%s %s %s\n%s"
      (keyw "match")
      (!. idtx c)
      (keyw "with") begin
      valt |> string_of_xlist "\n" @@ fun (va, u) ->
      sprintf "%s%s %s %s\n%s%s"
        idt (keyw "|") (formatted_of_varm va)
        (keyw "->") (idt ^ "  ") (!. (idtx + 1) u)
    end
  | Let (x, bde, body) ->
    sprintf "%s %s %s\n%s%s\n%s%s\n%s%s"
      (keyw "let") x (keyw "=")
      (idt ^ "  ")
      (!. (idtx + 1) bde)
      idt (keyw "in")
      idt (!. (idtx + 1) body)
  | Letrec (x, bde, body) ->
    sprintf "%s %s %s %s\n%s%s\n%s%s\n%s%s"
      (keyw "let") (keyw "rec") x (keyw "=")
      (idt ^ "  ")
      (!. (idtx + 1) bde)
      idt (keyw "in")
      idt (!. (idtx + 1) body)
  | Join (l, es, bde, body) ->
    sprintf "%s %s %s %s\n%s%s\n%s%s\n%s%s"
      (keyw "join") l (optional_brac (keyw"(", keyw")") es @@ concat (keyw ", ")) (keyw "=")
      (idt ^ "  ")
      (!. (idtx + 1) bde)
      idt (keyw "in")
      idt (!. (idtx + 1) body)
  | Joinrec (l, es, bde, body) ->
    sprintf "%s %s %s %s %s\n%s%s\n%s%s\n%s%s"
      (keyw "join") (keyw "rec") l (optional_brac (keyw"(", keyw")") es @@ concat (keyw ", ")) (keyw "=")
      (idt ^ "  ")
      (!. (idtx + 1) bde)
      idt (keyw "in")
      idt (!. (idtx + 1) body)
  | Jump (l, trs) ->
    sprintf "%s %s %s"
      (keyw "jump")
      l
      (optional_brac (keyw"(", keyw")") trs @@ string_of_xlist (keyw ", ") (!. (idtx + 1)))
  | Try(e, exc) ->
    sprintf "%s\n%s%s\n%s%s\n%s"
      (keyw "try")
      (makeidt (idtx + 1))
      (!. (idtx + 1) e)
      idt
      (keyw "with")
      begin
        exc |> string_of_xlist (keyw "\n") @@ fun (k, args, u) ->
        sprintf "%s%s %s %s %s %s"
          idt (keyw "|") (cstrw k) (string_of_xlist " " id args) (keyw "->")
          (!. idtx u)
      end
  | Exn(k, args) -> sprintf "%s%s" (cstrw k)
                      (optional_brac (keyw"(", keyw")") args @@ string_of_xlist (keyw ", ") (!. idtx))
  | Handlers hs -> sprintf "%s%s%s" (keyw "[") (string_of_xlist (keyw "; ") (fun (ex, h) -> sprintf "(%s, %s)" (cstrw ex) (!. idtx h)) hs) (keyw "]")
  | Append (xs, ys) -> sprintf "(%s) %s (%s)" (!. idtx xs) (keyw "@") (!. idtx ys)
  | Lookup(ex, hs) -> sprintf "lookup(%s, %s)" (cstrw ex) @@ (!. idtx hs)
and formattedstring_of_term t = formatted_of_term_idtx 0 t
and string_of_term =
  let (!.) = string_of_term in
  let infix e1 e2 op = sprintf "(%s) %s (%s)" (!. e1) op (!. e2) in
  function
  | True -> "true"
  | False -> "false"
  | Unit -> "()"
  | Var v -> v
  | Int i -> string_of_int i
  | Add(e1, e2) -> infix e1 e2 "+"
  | Sub(e1, e2) -> infix e1 e2 "-"
  | Mul(e1, e2) -> infix e1 e2 "*"
  | Div(e1, e2) -> infix e1 e2 "/"
  | Lt(e1, e2) -> infix e1 e2 "<"
  | Lamb(x, tr) -> sprintf "(fun %s -> %s)" x (!. tr)
  | App (fn, arg) -> sprintf "%s %s" (!. fn) (!. arg)
  | Data (k, es) ->
    sprintf "(%s%s)"
      k
      (optional_brac ("(", ")") es @@ string_of_xlist ", " (!.))
  | Match (c, alt) ->
    sprintf "match (%s) with %s"
      (!. c) @@
    string_of_xlist " " (fun (k, args, e) ->
        sprintf "| %s%s -> (%s)"
          k
          (string_of_armdata args)
          (!. e)) alt
  | MatchVal(c, valt) ->
    sprintf "match (%s) with %s"
      (!. c)
      begin
        valt |> string_of_xlist " " @@ fun (va, u) ->
        sprintf "| %s -> (%s)"
          (string_of_varm va)
          (!. u)
      end
  | Let (x, bde, body) -> sprintf "let%s in %s" (string_of_vb (x, bde)) (!. body)
  | Letrec (x, bde, body) -> sprintf "let rec%s in %s" (string_of_vb (x, bde)) (!. body)
  | Join (l, es, bde, body) -> sprintf "join %s in %s" (string_of_jb (l, es, bde)) (!. body)
  | Joinrec (l, es, bde, body) -> sprintf "join rec %s in %s" (string_of_jb (l, es, bde)) (!. body)
  | Jump (l, trs) -> sprintf "jump %s %s" l @@ optional_brac ("(", ")") trs @@ string_of_xlist ", " (!.)
  | Try(e, exc) ->
    let exc' = exc |> string_of_xlist "| " @@ fun (k, args, u) ->
      sprintf "%s %s -> %s" k (concat " " args) @@ !. u
    in sprintf "try %s with %s"
      (!. e)
      exc'
  | Exn(k, args) -> sprintf "%s%s" k
                      (optional_brac ("(", ")") args (string_of_xlist ", " (!.)))
  | Handlers hs -> sprintf "[%s]" @@ string_of_xlist "; " (fun (ex, h) -> sprintf "(%s, %s)" ex (!. h)) hs
  | Append (xs, ys) -> sprintf "(%s) @ (%s)" (!. xs) (!. ys)
  | Lookup(ex, hs) -> sprintf "lookup(%s, %s)" ex @@ !. hs
and rawstring_of_statement = function
  | Term t -> sprintf "Term(%s)" @@ rawstring_of_term t
  | LetAssign(x, bde) -> sprintf "LetAssign(\"%s\", %s)" x (rawstring_of_term bde)
  | LetrecAssign(x, bde) -> sprintf "LetrecAssign(\"%s\", %s)" x (rawstring_of_term bde)
  | Typedef(tyname, tvars, cstrs) ->
    let cstrs' = cstrs |> string_of_xlist "; " @@ fun (kstr, tys) ->
      sprintf "(\"%s\", [%s])" kstr @@ string_of_xlist "; " rawstring_of_type tys
    in sprintf "Typedef(\"%s\", [%s], [%s])" tyname (string_of_xlist "; " (fun x -> sprintf "\"%s\"" x) tvars) cstrs'
  | Exndef(k, args) -> sprintf "Exndef(\"%s\", [%s])" k (string_of_xlist "; " rawstring_of_type args)
and string_of_statement = function
  | Term t -> sprintf "%s;;" @@ string_of_term t
  | LetAssign(x, bde) -> sprintf "let%s;;" @@ string_of_vb (x, bde)
  | LetrecAssign(x, bde) -> sprintf "let rec%s;;" @@ string_of_vb (x, bde)
  | Typedef(tyname, tvars, cstrs) ->
    sprintf "type %s %s = %s;;" (string_of_tvars tvars) tyname (string_of_constructors cstrs)
  | Exndef(k, args) -> sprintf "exception %s of %s" k (string_of_xlist " * " string_of_type args)
and rawstring_of_value = function
  | VInt i -> sprintf "VInt(%d)" i
  | Abs(x, tr) -> sprintf "Abs(\"%s\", %s)" x @@ rawstring_of_term tr
  | VData(k, vs) -> sprintf "VData(\"%s\", [%s])" k @@string_of_xlist "; " rawstring_of_value vs
  | VExn(k, args) -> sprintf "VExn(\"%s\", [%s])" k (string_of_xlist "; " rawstring_of_value args)
  | VTrue -> "VTrue"
  | VFalse -> "VFalse"
  | VUnit -> "VUnit"
and formattedstring_of_value v =
  let rec inner idtx v =
    let makeidt idtx = String.make (idtx * 2) ' ' in
    match v with
    | VTrue | VFalse | VUnit | VInt _ -> consw @@ string_of_value v
    | Abs(x, tr) ->
      brace "(" ")" @@ sprintf "%s %s %s\n%s%s"
        (funw "fun") x (keyw "->")
        (makeidt @@ idtx + 1) (formatted_of_term_idtx idtx tr)
    | VData(k, vs) -> brace "(" ")" @@ sprintf "%s%s" (cstrw k) @@ optional_brac ("(", ")") vs @@ string_of_xlist (keyw ", ") @@ inner @@ idtx + 1
    | VExn(k, args) -> sprintf "%s%s" (cstrw k) @@
      optional_brac ("(", ")") args @@ string_of_xlist (keyw ", ") @@ inner @@ idtx + 1
  in inner 0 v
and string_of_value = function
  | VInt i -> string_of_int i
  | Abs (x, tr) -> sprintf "fun %s -> %s " x (string_of_term tr)
  | VData (k, vs) -> sprintf "(%s%s)" k @@ string_of_xlist ", " string_of_value vs
  | VExn(k, args) -> sprintf "%s %s" k (string_of_xlist " " string_of_value args)
  | VTrue -> "true"
  | VFalse -> "false"
  | VUnit -> "()"
and rawstring_of_joel joel =
  sprintf "[%s]" @@ string_of_xlist "; " rawstring_of_statement joel
and formattedstring_of_joel joel =
  sprintf "[%s]" @@ string_of_xlist "\n" string_of_statement joel
and string_of_joel joel =
  sprintf "[%s]" @@ string_of_xlist " " string_of_statement joel
(* }}} *)

