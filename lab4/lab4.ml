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
  let col = List.fold_right (fun x a -> if List.length x > a then List.length x else a) l 0 in
  row = col

(*adds two matices. If the two input matrices are not the same size, the behavior is unspecified*)
let add_matices l l' =
  List.fold_right (fun x a -> x) l []