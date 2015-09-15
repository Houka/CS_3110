(*===Map, fold, and filter excercises====*)
(*returns product of list of floats*)
let product l =
  List.fold_right (fun x a -> x *. a) l 1.0

(*find max in list of floats*)
let max l =
  List.fold_right (fun x a -> if x>a then x else a) l (List.hd l)

(*Add 1.0 to every element of a list of floats*)
let add1 l =
  List.fold_right (fun x a -> x +. 1.0 :: a) l []

(*Find those elements of a list of floats that are greater than 0.0.*)
let find_pos_int l =
  List.filter (fun x -> x>0.0) l

(*====Recursion, folding, lib functions====*)
(*Returns whether all elements of the list are true*)
(*Recursion*)
let rec lst_and = function
  | [] -> true
  | h::t -> if h then lst_and t else false
(*using fold*)
let lst_and_fold l =
  List.fold_right (fun x a -> x && a) l true
(*using List but not fold*)
let lst_and_other l =
  let result = List.filter (fun x -> x=false) l in
  if List.length result = 0 then true else false

(*returns whether at least one element of the list satisfies the predicate p*)
(*Recursion*)
let rec exists p = function
  | [] -> false
  | h::t -> if p h then true else exists p t
(*using folds*)
let exists_fold p l =
  List.fold_right (fun x a -> p x || a) l false
(*using List but no fold*)
let exists_other p l =
  List.exists (fun x -> p x) l

(*====Matrices====*)
(*returns whether the input matrix is square.
*A matrix is square if the number of rows is equal to the number of columns.*)
let is_square l =
  let row = List.length l in
  List.fold_right ( fun col a -> a && row = (List.length col)) l true


(*adds two matices. If the two input matrices are not the same size, the behavior is unspecified*)
let add_matices l l' =
  List.map2 (fun x y -> List.map2 (fun a b -> a+b) x y) l l'

(*
Implement a function multiply_matrices:
int list list -> int list list -> int list list
that returns the matrix product of the two input matrices.
If the two input matrices are not of sizes that can be multiplied together,
the behavior is unspecified.
*)
let col_nth l n = List.fold_right (fun x a -> List.nth x n :: a) l []
let rec flip_matrix l n=
  if n > 0 then
    col_nth l (n-1) :: flip_matrix l (n-1)
  else
    []
let dot_product l l' =
  let multiplied_list = (List.map2 (fun x y -> x*y) l l') in
  let result = List.fold_right (fun x a -> x+a) multiplied_list 0 in
  result
let dot_product_list l matrix =
  List.fold_left (fun a x -> dot_product l x :: a) [] matrix
let multiply_matrices l l' =
  let flipped = flip_matrix l' (List.length (List.hd l')) in
  List.fold_right (fun x a -> dot_product_list x flipped::a) l []



