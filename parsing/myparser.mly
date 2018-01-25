%{
open Syntax

let rec lastpop =
  function
  | (_, []) -> failwith "no item"
  | (ls, x :: []) -> (List.rev ls, x)
  | (ls, x :: ls') -> lastpop (x :: ls, ls')

let (<::>) a b = Data("Cons", [a; b])

let make_data_ls ls =
  let rec work xs k =
    match xs with
    | [] -> k @@ Data("Nil", [])
    | x :: xs' -> work xs' @@ fun xs0 -> x <::> (k xs0)
  in work (List.rev ls) (fun i -> i)

let typlist = ref []
let exnlist = ref []
let fresh_var =
  let open Common in
  fun s ->
  Printf.sprintf "%s%d" s @@ upcounter ()
%}

%token LPAREN RPAREN LT GT LSQBR RSQBR BEGIN END
%token TRUE FALSE
%token ARROW FUN FUNCTION MATCH TRY WITH BRANCH TYPEKEY OF
%token EXCEPTION
%token FORALL DOT COMMA
%token SEMICOLS SEMICOL COLS
%token LET REC IN JOIN JUMP EQ ASTER
%token ADD SUB DIV
%token RAISE
%token <int> INT
%token <string> VAR
%token <string> CONSTR
%token EOF

%start <Syntax.joel> parse
%%

parse: stat EOF { $1 }

braced(X):
  | X { $1 }
  | BEGIN x = braced(X) END { x }
  | LPAREN x = braced(X) RPAREN { x }

optparam(X):
  | LPAREN x = X RPAREN { Some(x) }
  | { None }

opttparan(X):
  | LT x = X GT { Some(x) }
  | { None }

stat:        | e = exp SEMICOLS s = stat { (Term e) :: s }
             | td = typedef SEMICOLS s = stat { td :: s }
             | LET REC l = vb SEMICOLS s = stat {
               let (x, bde) = l in
               (LetrecAssign(x, bde)) :: s }
             | LET l = vb SEMICOLS s = stat {
               let (x, bde) = l in
               (LetAssign(x, bde)) :: s }
             | exp { [Term $1] }
             | exp SEMICOLS { [Term $1] }
             | typedef { [$1] }
             | typedef SEMICOLS { [$1] }
             | LET l = vb {
               let (x, bde) = l in
               [LetAssign(x, bde)] }
             | LET l = vb SEMICOLS {
               let (x, bde) = l in
               [LetAssign(x, bde)] }
             | LET REC l = vb {
               let (x, bde) = l in
               [LetrecAssign(x, bde)] }
             | LET REC l = vb SEMICOLS {
               let (x, bde) = l in
               [LetrecAssign(x, bde)] }
             | EXCEPTION ex = CONSTR OF tys = monotparams SEMICOLS s = stat {
               exnlist := ex :: !exnlist;
               Exndef(ex, tys) :: s }
             | EXCEPTION ex = CONSTR SEMICOLS s = stat {
               exnlist := ex :: !exnlist;
               Exndef(ex, []) :: s }
             | EXCEPTION ex = CONSTR OF tys = monotparams SEMICOLS {
               exnlist := ex :: !exnlist;
               [Exndef(ex, tys)] }
             | EXCEPTION ex = CONSTR SEMICOLS {
               exnlist := ex :: !exnlist;
               [Exndef(ex, [])] }

fnable_exp:  | var = VAR { Var var }
             | LPAREN e = exp RPAREN { e }
             | BEGIN e = exp END { e }
             | FUN vs = nonempty_list(VAR) ARROW e = exp {
                 List.fold_right (fun v s -> Lamb(v, s)) vs e }
             | FUNCTION a = alt {
               let x = fresh_var "x" in
               Lamb(x, Match(Var x, a)) }
             | FUNCTION a = valt {
               let x = fresh_var "x" in
               Lamb(x, MatchVal(Var x, a)) }
             | fn = fnable_exp arg = arg_exp { App (fn, arg) }

arg_exp:     | RAISE ex = braced(exncst) { App(Var"raise", ex) }
             | INT { Int $1 }
             | VAR { Var $1 }
             | LPAREN e = exp RPAREN { e }
             | BEGIN e = exp END { e }
             | LPAREN RPAREN { Unit }
             | TRUE { True }
             | FALSE { False }
             | constructor { $1 }
             | MATCH e = exp WITH a = alt { Match (e, a) }
             | MATCH e = exp WITH a = valt { MatchVal (e, a) }
             | TRY e = exp WITH ex = exc { Try(e, ex) }
             | LET l = vb IN body = exp {
               let (x, bde) = l in
               Let(x, bde, body) }
             | LET REC l = vb IN body = exp {
               let (x, bde) = l in
               Letrec(x, bde, body) }
             | JOIN jb = jb IN body = exp {
               let (j, xs, bde) = jb in
               Join(j, xs, bde, body) }
             | JOIN REC jb = jb IN body = exp {
               let (j, xs, bde) = jb in
               Joinrec(j, xs, bde, body) }
             | JUMP l = VAR es = optparam(explist) {
               match es with
               | Some(es') -> Jump(l, es')
               | None -> Jump(l, []) }
             | lst { $1 }

exp:         | arg_exp { $1 }
             | fnable_exp { $1 }
             | e1 = exp ADD e2 = exp { Add(e1, e2) }
             | e1 = exp SUB e2 = exp { Sub(e1, e2) }
             | e1 = exp ASTER e2 = exp { Mul(e1, e2) }
             | e1 = exp DIV e2 = exp { Div(e1, e2) }
             | e1 = exp LT e2 = exp { Lt(e1, e2) }
             | x = exp COLS xs = exp { x <::> xs }

lst:         | LSQBR ls = separated_list(SEMICOL, exp) RSQBR {
               make_data_ls ls }

vb:          | x = VAR EQ bde = exp { (x, bde) }

(* tvars:       | separated_list (COMMA, xvar) { $1 } *)
jb:          | l = VAR  xargs = optparam(datargs) EQ bde = exp {
               match xargs with
               | Some xargs' -> (l, xargs', bde)
               | None -> (l, [], bde) }

typlist:     | separated_list(COMMA, typ) { $1 }
explist:     | separated_list(COMMA, exp) { $1 }
varm:        | BRANCH v = vcase ARROW e = exp { (v, e) }
valt:        | nonempty_list(varm) { $1 }
vcase:       | INT { AInt $1 }
             | TRUE { ATrue }
             | FALSE { AFalse }
             | xvar { AVar $1 }

xlist:       | separated_list(COMMA, VAR) { $1 }
excarm:        | BRANCH k = CONSTR es = optparam(xlist) ARROW e = exp {
               match es with
               | Some(es') -> (k, es', e)
               | None -> (k, [], e) }

exc:         | nonempty_list(excarm) { $1 }
dataconstr:  | k = CONSTR a = optparam(datargs) {
               match a with
               | Some a' -> (k, a')
               | None -> (k, []) }
             | x = VAR COLS xs = VAR { ("Cons", [x; xs]) }
             | LSQBR RSQBR { ("Nil", []) }

datargs:     | separated_list(COMMA, VAR) { $1 }
constructor: | k = CONSTR es = optparam(explist) {
               match es with
               | Some es' -> Data(k, es')
               | None ->
                 if List.exists ((=) k) !exnlist then Exn(k, [])
                 else Data(k, []) }

arm:         | BRANCH d = dataconstr ARROW e = exp {
               let (k, args) = d in (k, args, e) }
alt:         | nonempty_list(arm) { $1 }

exncst:      | ex = CONSTR es = optparam(explist) {
               match es with
               | Some es' -> Exn(ex, es')
               | None -> Exn(ex, []) }

xvar:        |  VAR { $1 }
             | CONSTR { $1 }

typ:         | LPAREN t = typ RPAREN { t }
             | v = xvar {
               if List.exists (fun k -> k = v) !typlist then
                 TDataRef(v, [])
               else match v with
                 | "int" -> TInt
                 | "unit" -> TUnit
                 | "base" -> Base
                 | "bool" -> Bool
                 | _ -> TVar v }
             | t1 = typ ARROW t2 = typ { Arrow (t1, t2) }
             | tname = xvar LT tys = typlist GT { TDataRef(tname, tys) }
             | t1 = typ t2 = typ { Ttapp (t1, t2) }
             | FORALL v = xvar DOT t = typ {Univ (v, t) }

tparams:     | list(VAR) { $1 }
cparams:     | separated_nonempty_list(ASTER, typ) { $1 }
monotparams: | separated_nonempty_list(ASTER, typ) { $1 }
tarms:       | BRANCH cstr = CONSTR OF cp = cparams ta = tarms { (cstr, cp) :: ta }
             | BRANCH cstr = CONSTR ta = tarms { (cstr, []) :: ta } 
             | BRANCH cstr = CONSTR OF cp = cparams { [(cstr, cp)] }
             | BRANCH cstr = CONSTR { [(cstr, [])] }

typedef:     | TYPEKEY tp = tparams EQ ta = tarms {
               let tp, tyname = lastpop ([], tp) in
               typlist := tyname :: !typlist;
               Typedef(tyname, tp, ta) }
