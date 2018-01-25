let pervasives = [
  "open Pervasives;;";
  "type 'a mylist =
       | Nil
       | Cons of 'a * 'a mylist;;";
  "type 'a option =
      | Some of 'a
      | None;;";
  "type mybool =
      | True
      | False;;";
  "exception Not_found;;";
  "let less a b = if a < b then True else False;;";
  "let (<) a b = less a b;;";
  (* CPS module {{{ *)
  "module C = struct
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
         | Nil -> failwith \"not found\"
     end;;";
  (* }}} *)
  (* for stream {{{ *)
  "type ('a, 'b) stream_shape =
       | Empty
       | Block of 'a * 'b;;";
  "type _ stream = E : 'b * ('b -> ('a, 'b) stream_shape) -> 'a stream;;";
  "type ('a, 'b) pair = P of 'a * 'b;;";
  (* }}} *)
]
