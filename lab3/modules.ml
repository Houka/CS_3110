(* excercise about indentation *)
(*
* 1. let x = 0 in x
*       => 0
* 2. let x = let x = 0 in x in x
*       => 0
* 3. let x = 0 in let f x = x in let x = 1 in f 2
*       => 2
* 4. let x = 0 in let f y = x in let x = 1 in f 2
*       => 0
* 5. let x = let y = 0 in y in let z = 1 in let w = 2 in x + y + z + w
*       => error unbound value y
*
*     3 and 4 does not violate principle of name irrelevance because the x in
*     let f x = x is in scope of the argument thus f 2 = 2
*)

(*Defining modules*)
module MyModule = struct
  let x = 3
  let f y = x+y
end

(*using functions from modules*)
MyModule.f 3
(*finding type of module*)
module type T = module type of MyModule

(*Filename has dir_sep that is used to separate file names*)

(*to bring in all definitions in a module use*)
open MyModule   (*now we can use f 3 instead of MyModule.f 3*)

(*Opening in limited scope is... let open MyModule in *)


(*Types of ways to open modules*)
String.capitalize

module S = String
S.capitalize

open String
capitalize