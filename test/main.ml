let () = ignore @@ List.map (fun f -> f ()) [
    Parsetest.test;
    Extest.test;
    Cpsconv.test;
    CpsFullproc.test;
    Contify.test;
    Optimizer.test;
    JoelFullproc.test;
  ]
