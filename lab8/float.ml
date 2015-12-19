type t = float
let zero = 0.0
let one = 1.0
let (+) a b = a +. b
let ( * ) a b = a *. b
let (~-) a = 1.0 *. a

(* This error is caused by the fact that Floats.(/) is not visible,
 so the toplevel is interpreting (/) as a plain old integer division: *)
let (/) a b = a /. b (* does not work cause not defined in sig *)

let to_string a = match a with
  | x -> (string_of_float x)