(* Modules - recitation 8 *)
#use "queues.ml"
(* Exercise: Create a ListQueue that contains the integers 1, 2, and 3 in that order.
*)
let a = ListQueue.enqueue 1 (ListQueue.enqueue 2 (ListQueue.enqueue 3 (ListQueue.empty)))

(*Exercise: Read the implementation of ListQueue.enqueue.
Explain in your own words why its efficiency is linear time in the length of
the queue.

- Ans: it takes an element and walks down all of the ListQueue to put the
element in the end. [@] symbol does this
*)

(*Exercise: Use the following code to create ListQueue's of
exponentially increasing length: 10, 100, 1000, etc.
How big of a queue can you create before there is a noticeable delay?
How big until there's a delay of at least 15 seconds?
(Note: you can abort utop computations with Ctrl-C.)

- Ans: notible delay at 10000
*)
(* Creates a ListQueue filled with [n] elements. *)
let fill_queue n =
  let rec loop n q =
    if n=0 then q
    else loop (n-1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty

(* Exercise: Repeat the above three exercises with TwoListQueue,
whose enqueue operation is constant time.
You will need to modify fill_queue to use TwoListQueue instead of ListQueue.
*)
let b = TwoListQueue.enqueue 1 (TwoListQueue.enqueue 2
          (TwoListQueue.enqueue 3 (TwoListQueue.empty)))
let fill_two_queue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n-1) (TwoListQueue.enqueue n q) in
  loop n TwoListQueue.empty
(* delay at 10,000,000 *)



(* Arithmetic *)
(* Exercise: In your working file for the recitation,
  define a module type called Arith containing

a type t
a value called zero
a value called one
a function (+) : t -> t -> t
a function ( * ) : t -> t -> t
a function (~-) : t -> t (note that ~- is how ocaml writes unary negation).
*)
module type Arith = sig
  type t
  val zero : t
  val one : t
  val (+) : t -> t -> t
  val ( * ) : t -> t -> t
  val (~-) : t -> t
  val to_string : t -> string
end

(* Exercise: In your working file for the recitation, define a module called
Ints that implements the Arith module type: *)
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

(* Exercise: In your working file for the recitation, define a module called
Floats that also implements the Arith signature.
Include an additional function called (/) for dividing
two floats.
 *)
module Floats : Arith = struct
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
end

(* Exercise: Which of the following expressions are valid? Keep in mind that
when type checking expressions, the compiler only looks at the module type of
the module, and not the module itself.

Ints.(one + one)            pass
Ints.(1 + 1)                error
Floats.(one + one)          pass
Floats.(one +. one)         error
Floats.(1 + 1)              error
Floats.(1. + 1.)            error
Floats.(1. +. 1.)           pass
Ints.(1. +. 1.)             pass
Floats.(zero / one)         error
 *)

(* Exercise: It is sometimes sensible to provide a conversion from your abstract
type to or from some "lowest common denominator" type. Which of the following
functions would make sense as part of the Arith interface? Add the sensible ones.

to_int : t -> int
of_int : int -> t
to_float : t -> float
of_float : float -> t
Think about the impact of adding these functions on other modules you might
want to implement in the future. *)
(* of_float does not work as well *)

(* Adding type info with type constraints *)
module type ArithWithInt = Arith with type t = int
module Ints' : Arith with type t := int = struct
  let zero = 0
  let one = 1
  let (+) a b = a + b
  let ( * ) a b = a * b
  let (~-) a = -a
  let to_string x = (string_of_int x)
end
(* Exercise: With your new implementation, which of these expressions are valid? Test your answers.

Ints.(one + one)      pass
Ints.(1 + 1)          pass
Ints.(1. +. 1.)       pass
 *)

(* There is a slightly different way to express type constraints, which uses
with type t := ... instead of with type t = ...
(the difference is the := instead of =). Instead of defining type t,
the := syntax removes t and replaces it everywhere. It is occasionally useful,
for example to resolve errors where a type is defined multiple times.

Exercise: At the toplevel, compare the signatures Arith with type t = int and
Arith with type t := int.
*)


(* Including/extending module types *)
(* Exercise: with the following module type definitions,
what values must be defined by a module of type T'? *)
module type T = sig
  val x : int
end

module type T' = sig
  include T
  val y : int
end
(* T' must define x and y *)

(* Exercise: Modify your implementation of Floats so that the (/) operator is visible. *)

module Floats' : sig
  include Arith
  val (/) : t -> t -> t
end = struct
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
end

(* Exposing everything in a module *)


(* Exercise: what is the module type of the following module:
  module X = struct let x = 0 let f y = [y] end?

- Ans = It is it's own time
*)

(* Dictionaries *)
module type Dictionary = sig
  type ('k, 'v) t

  (** The empty dictionary *)
  val empty  : ('k, 'v) t

  (** insert k v d produces a new Dictionary.t with the same mappings as d and
      also a mapping from k to v.  If k was already mapped in d, the old mapping
      is replaced. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  (** lookup k d returns the value associated with k in d.  It raises an
      exception if there is no binding. *)
  val lookup  : 'k -> ('k, 'v) t -> 'v
end

(* Exercise: write a module AssocList that implements the Dictionary interface
using type ('k,'v) t = ('k * 'v) list.
Note that the List module contains some convenient functions for working with
association lists. *)
module AssocList : Dictionary = struct
  type ('k, 'v) t = ('k * 'v) list
  let empty = []
  let insert k v l = (k, v) :: l
  let lookup k l = snd (List.find (fun x -> fst x = k) l)
end

(* Exercise: write a module BSTDict that implements the Dictionary
interface using the bstree type that you defined in the last lab. *)
type ('k, 'v) bstree = Leaf | Data of 'k * 'v * ('k , 'v ) bstree * ('k , 'v ) bstree
module BSTDict : Dictionary with type ('k, 'v) t = ('k , 'v) bstree = struct
  type ('k, 'v) t = ('k , 'v) bstree
  let empty = Leaf
let rec lookup (key: 'k) (tree: ('k,'v) bstree) : 'v =
  match tree with
  | Leaf -> failwith "Key Not Found"
  | Data (k,v,l,r) -> if key = k then v else if key < k then lookup key l else lookup key r
let rec insert (key: 'k) (v: 'v) (tree: ('k, 'v) bstree) : ('k, 'v) bstree =
  match tree with
  | Leaf -> Data (key, v, Leaf, Leaf)
  | Data (k,v,l,r) -> if key < k then
                        let node = (insert key v l) in
                        Data (k,v,node,r)
                      else
                        let node = insert key v r in
                        Data (k,v,l,node)
end





