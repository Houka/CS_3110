(* Functors - recitation 9 *)
type date = int * int * int (* (year, month, day) *)

(* Write a module Date that implements the Map.OrderedType with type t = date.

Exercise: Use the Map.Make functor with your Date module to create a DateMap module. *)
module Date: Map.OrderedType with type t = date = struct
  type t = date

  let is_before ((y1,m1,d1):date) ((y2,m2,d2):date) : bool =
    if y1 < y2 then true
  else if m1 < m2 then true
  else if d1 < d2 then true
  else false

  let is_equal ((y1,m1,d1):date) ((y2,m2,d2):date) : bool =
    y1 = y2 && m1 = m2 && d1 = d2

  let compare a b = if is_before a b then -1 else if is_equal a b then 0 else 1
end

module DateMap = Map.Make(Date)

(* Exercise: Using the functions in the DateMap module, create a calendar
with four entries in it. *)
type calendar = string DateMap.t
let a = DateMap.(add (1,2,3) "asd" empty)
let b = DateMap.(add (2,2,3) "asd" a)
let c = DateMap.(add (3,2,3) "asda" b)
let four : calendar = DateMap.(add (4,2,3) "asd" c)

(* Exercise: Write a function first_after : calendar -> Date.t -> string
  that returns the name of the first event that occurs after the given date (hint: use split). *)
let first_after (c: calendar) (t : Date.t) : string =
  let spliter = DateMap.split t c in
  match spliter with
  | (_,_,c) -> snd (DateMap.min_binding c)

(* Exercise: Write a functor called ExtendArith that takes a module A of type
Arith and produces a module with the following functions:

(-) : A.t -> A.t -> A.t
of_int : int -> A.t
*)
module type Arith = sig
  type t
  val zero  : t
  val one   : t
  val (+)   : t -> t -> t
  val (~-)  : t -> t
  val ( * ) : t -> t -> t

  val to_string : t -> string
end
module Ints : Arith = struct
  type t = int
  let zero = 0
  let one = 1
  let (+) a b = a + b
  let ( * ) a b = a * b
  let (~-) a = -a
  let to_string a = match a with
    | x -> (string_of_int x)
end
module ExtendArith (A : Arith) = struct
  include A
  include Pervasives
  let rec of_int (a:int) : A.t =  A.(if a = 0 then zero else one + of_int (a - 1))
  let (-) a b = A.(+) a (A.(~-) b)
end

(* Exercise: use "include" to reimplement Ints using only 3 lines
  (hint: what module already defines (+), (~-), etc.?): *)
module Ints' : Arith with type t := int = struct
  include Pervasives
  let zero = 0
  let one = 1
  let to_string a = string_of_int a
end

(* Exercise: modify your definition of ExtendArith to include
the definitions of the module being extended. *)

(* Exercise: Implement a functor called Fractions that takes a module A of
type Arith and produces a module that implements Arith using fractions.
The produced modules should also include a (/) function for division: *)
module Fractions (A : Arith) = struct
  let t = A.t*A.t
  let rec of_int (a:int) : A.t =  A.(if a = 0 then zero else one + of_int (a - 1))

  let (/) a b = (a,b)
end















