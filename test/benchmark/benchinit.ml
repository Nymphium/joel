(* header {{{ *)
type 'a mylist =
  | Nil
  | Cons of 'a * 'a mylist;;
let rec of_list = function
  | [] -> Nil
  | x :: xs -> Cons(x, of_list xs);;
(* CPS module {{{ *)
module C = struct
  type ('k, _, _) assoc_hlist =
    | Nil  : ('k, 'z, 'z) assoc_hlist
    | Cons : ('k * 't) * ('k, 'u, 'x) assoc_hlist -> ('k, 't -> 'u, 'x) assoc_hlist
  let rec (@) : type k ty1 ty2 v.
    (k, ty1, ty2) assoc_hlist ->
    (k, ty2, v) assoc_hlist ->
    (k, ty1, v) assoc_hlist  = fun a b ->
    match a with
    | Nil -> b
    | Cons(x, xs) -> Cons(x, xs @ b)
  let rec lookup : type k a b.
    k -> (k, a, b) assoc_hlist -> b = fun key -> function
    | Cons((key', x), ls) -> if key = key' then Obj.magic x else lookup key ls
    | Nil -> raise Not_found
end;;
(* }}} *)
(* for stream {{{ *)
type ('a, 'b) stream_shape =
  | Empty
  | Block of 'a * 'b;;
type _ stream = E : 'b * ('b -> ('a, 'b) stream_shape) -> 'a stream;;
type ('a, 'b) pair = P of 'a * 'b;;

let of_mylist =
  let rec list_length = function
    | Nil -> 0
    | Cons(_, xs) -> 1 + (list_length xs)
  in
  let rec list_nth = fun i -> function
    | Nil -> raise Not_found
    | Cons(x, xs) ->
      begin match i with
        | 0 -> x
        | _ -> list_nth (i - 1) xs
      end
  in
  let step = fun p ->
    match p with
    | P(i, lst) ->
      match i < list_length lst with
      | true -> Block(list_nth i lst, (P(i + 1, lst)))
      | false -> Empty
  in fun lst -> E(P(0, lst), step)
(* }}} *)
(* }}} *)
