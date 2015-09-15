(*print funtions*)

Printf.printf "hello %s (version %d) \n" "world" 1
  (*prints: hello world (version 1) *)

Printf.printf "%g%% %B\n%g%% %B\n100%% %s!\n" 95.0 true 5.0 false "ocaml"

(*side effects*)
let lightning_storm _ =
  let f x y = print_endline "A"; x + y in

  (print_endline "B"; f) (print_endline "C"; 0) (print_endline "D"; 1)
    (*output:
        D
        C
        B
        A
        1
    *)
let corrupt_padowan _ =
  let f x y = print_endline "A"; x + y in
  let g     = print_endline "B"; f in

  g (print_endline "C"; 0) (print_endline "D"; 1)
    (*output:
        B
        D
        C
        A
        int = 1
    *)