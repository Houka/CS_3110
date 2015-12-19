(*
Exercise: Give a type definition and a value of that type for each of the
following:

A set of sets, where the type of elements can by anything, and where sets are
 represented by lists.

A list of triples, the first two components of which have the same type,
 and the third component of which is of some possibly different type.

The suit of a card deck.

A type whose values are "things", where a "thing" is either an integer or
 a list of "things".

A type whose values are pairs whose components can be of any type,
 as long as they are of the same type.
*)

type 'a set_of_set = 'a list list
let a :int set_of_set = [[3];[2];[1]]

type ('a, 'b) triples = ('a* 'a * 'b) list
let b : (int,string) triples = [(1,2,"asd");(2,3,"gaf")]

type suit = Hearts | Clubs | Diamond | Spades
let c : suit = Hearts

type things = Thing of int | Things of int list
let d : things = Thing 1
let e : things = Things [1;2;3]

type 'a pairs = 'a * 'a
let f : int pairs = (1,2)

(* Exercise: a binary search tree is made up of nodes.
Each node is either a data node, which contains a key, a value,
a left child, and a right child, or it is a leaf node, which contains
nothing at all. Complete the following type definition:
*)
type ('k, 'v) bstree = Leaf | Data of 'k * 'v * ('k , 'v ) bstree * ('k , 'v ) bstree
let g : (int,int) bstree = Leaf
let h : (int,int) bstree = Data (1,2,g,g)
let i : (int,int) bstree = Data (1,2,h,h)
let i' : (int,int) bstree = Data (1,2,Data (0,2,g,g), Data (3,2,g,g))

(* Exercise: Write a function lookup : 'k -> ('k, 'v) bstree -> 'v
that returns the value associated with the given key,
and fails if the key is not in the tree.
*)
let rec lookup (key: 'k) (tree: ('k,'v) bstree) : 'v =
  match tree with
  | Leaf -> failwith "Key Not Found"
  | Data (k,v,l,r) -> if key = k then v else if key < k then lookup key l else lookup key r

(*Exercise: Write a function insert : 'k -> 'v -> ('k, 'v) bstree -> ('k, 'v)
bstree that adds a new key-value mapping to the given binary search tree. *)
let rec insert (key: 'k) (v: 'v) (tree: ('k, 'v) bstree) : ('k, 'v) bstree =
  match tree with
  | Leaf -> Data (key, v, Leaf, Leaf)
  | Data (k,v,l,r) -> if key < k then
                        let node = (insert key v l) in
                        Data (k,v,node,r)
                      else
                        let node = insert key v r in
                        Data (k,v,l,node)

(*  For example, the following patterns all match the expression [Some 3110; None]:

h :: tl                       []
(Some x)::tl                  [None]
[Some 3110; None]             [Some 2; None]
[Some x; _]                   [Some 2; Some 3; Some 4]
h1::h2::tl                    [Some 2]
h1::h2::[]                    [Some 2; Some 3; Some 4]

Exercise: for each pattern in this list, give a value of type int option list
that does not match the pattern.
*)

(* Exercise: Complete the following function, which should return true
if the length of the argument is odd: *)
let rec length_is_odd l = match l with
  | [] -> false
  | h::[] -> true
  | h1::h2::t -> length_is_odd t

(* Exercise: Complete quadrant function below, which should return the quadrant
of the given x, y vector, according to the diagram on the right
(borrowed from Wikipedia):
*)
let quadrant (x,y) = match (x,y) with
  | (x,y) when x>0 && y>0 -> 1
  | (x,y) when x<0 && y>0 -> 2
  | (x,y) when x<0 && y<0 -> 3
  | (x,y) when x>0 && y<0 -> 4
  | (_,_) -> 0

let quadrant2 (x,y) = match (x>0,y>0) with
  | (true,true) -> 1
  | (false,true)-> 2
  | (false,false) -> 3
  | (true,false) -> 4

(*Exercise: Write a function is_bst : ('k,'v) bstree -> bool that
returns true if the given tree satisfies the binary search tree property.
You may find it helpful to write a recursive helper function that takes a tree
and either gives you (i) the minimum and maximum value in the tree,
or (ii) tells you that the tree is empty, or (iii) tells you that the tree
is not well formed.

What would be a good return type for that helper?
*)
let rec is_bst (tree: ('a,'b) bstree ) : bool =
  let get_key (t: ('a,'b) bstree) : 'a =
    match t with
    | Leaf -> 0
    | Data (k,v,_,_) -> k in
  match tree with
  | Leaf -> true
  | Data (k,v,Leaf,Leaf) -> true
  | Data (k,v,Leaf, r) -> k < get_key r && is_bst r
  | Data (k,v,l,Leaf) -> k > get_key l && is_bst l
  | Data (k,v,l,r) -> (k < get_key r && is_bst r) &&
                      (k > get_key l && is_bst l)


(* Polymorphic Variants / Anonymous variants*)
(* Polymorphic variants are just like plain-old variants, except:

1.  You don't have declare them before using them
2.  They don't have names
3.  The constructors start with ` (this is a back tick,
    typically found on the same key as the ~ character, near the escape key).

*)
(* ex of polymorphic variants *)
let my_function x = match x with
  | 0 -> `Infinity
  | 1 -> `Finite 1
  | n -> `Finite (-n)

(* Expressions ( * ) *)
(* Exercise: Write a variant type expr suitable for representing arithmetic
expressions with integer constants. *)
type expr = Int of int
            | Neg of expr
            | Times of expr * expr
            | Plus of expr * expr
(* Exercise: Write a function eval : expr -> int that computes the
value of an arithmetic expression. *)
let rec eval (exp : expr): int =
  match exp with
  | Int (i) -> i
  | Neg (i) -> (-1) * (eval i)
  | Times (a,b) -> (eval a) * (eval b)
  | Plus (a,b) -> (eval a) + (eval b)
(* Exercise: Write a function normalize : expr -> expr that replaces all \
negative constants with Neg applied to a positive constant. For example, *)
let rec normalize (exp: expr) : expr =
  match exp with
  | Int (i) -> if i<0 then Neg (Int (-i)) else Int (i)
  | Neg (i) -> normalize i
  | Times (a,b) -> Times (normalize a, normalize b)
  | Plus (a,b) -> Plus (normalize a, normalize b)
(* Exercise: Write a function remove_negs : expr -> expr that removes
all Neg constructors by pushing them all the way down to the constants. *)
let rec remove_negs (exp: expr) : expr =
  match exp with
  | Int (i) -> Int (i)
  | Neg (Int (i)) -> Int (-i)
  | Neg (Neg h) -> remove_negs h
  | Neg (Plus (a,b))-> remove_negs (Plus (Neg a, Neg b))
  | Neg (Times (a,b))-> remove_negs (Times (Neg a, Neg b))
  | Times (a,b) -> Times (remove_negs a, remove_negs b)
  | Plus (a,b) -> Plus (remove_negs a, remove_negs b)




