open Genutils
open Extract
open Testcommon
open Printf

let output_file = "benchtests.ml"

let testf s = Printf.sprintf "fun () ->\n%s" s

let each_lines file f =
  try
    while true; do
      f @@ input_line file
    done;
  with End_of_file -> ()

let () =
  let file = open_out output_file in
  let template_create name lang size test =
    fprintf file "list := (create ~name: (Printf.sprintf \"%%10s (%%6s) (%%3d)\" \"%s\" \"%s\" @@ %d) (%s)) :: !list;\n\n"
      name lang size test
  in
  fprintf file "open Benchutils;;\n";
  fprintf file "open Benchconfig;;\n";
  begin
    let benchinit = open_in "benchinit.ml" in
    each_lines benchinit (fprintf file "%s\n")
  end;
  fprintf file "let list, size = ref [], ref [];;\n";
  begin Benchlist.list |>
        List.iter begin fun (name, test) ->
          let coreml = Coremlset.proc test in
          let joel = Joelset.proc test in
          let cps = Cpsset.proc test in
          fprintf file "(* %s {{{\n\n%s\n *)\n" name test;
          fprintf file "size := (\"%s\", %d, %d, %d) :: !size;;\n" name (estimate_size coreml) (estimate_size joel) (estimate_size cps);
          Benchconfig.arg_lists |>
          List.iteri begin fun j __arg__ ->
            let len = List.length __arg__ in
            fprintf file "(* %d {{{ *)\n" len;
            fprintf file "let __arg__ = List.nth arg_lists %d |> of_list in\n" j;
            template_create name "coreml" len @@ testf @@ extract_of_term coreml;
            template_create name "joel" len @@ testf @@ extract_of_term joel;
            template_create name "cps" len @@ testf @@ extract_of_term cps;
            fprintf file ";;\n(* }}} *)\n"
          end;
          fprintf file "(* }}} *)\n\n"
        end
  end;
  fprintf file "let list = List.rev !list;;\n";
  fprintf file "let size = List.rev !size;;\n";

