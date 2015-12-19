type t = int
let zero = 0
let one = 1
let (+) a b = a + b
let ( * ) a b = a * b
let (~-) a = -a
let to_string a = match a with
  | x -> (string_of_int x)
