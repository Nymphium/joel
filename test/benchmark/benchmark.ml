open Benchutils

let () =
  set_sample 10;
  set_batch 100;
  bench Benchtests.list;
  print_codesize Benchtests.size;
  print_endline "%%%%%"

