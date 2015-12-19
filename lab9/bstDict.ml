
(**
WARNING: this code comes from a debugging lab, IT CONTANS BUGS
**)

(******************************************************************************)
(** helper functions **********************************************************)
(******************************************************************************)

(** comparison_result and compare are more useful than < and > because we can
    pattern match on them instead of using if/then/else. *)
type comparison_result = Lt | Eq | Gt

let compare x y =
  if      x < y then Lt
  else if x = y then Eq
  else Gt


(******************************************************************************)
(** binary search tree type ***************************************************)
(******************************************************************************)


(** We store the dictionary as a binary search tree.  Each node contains a key
 *  and a value, and we maintain the BST invariant:
 *    - for every node n, every key in the left subtree of n is less than the
 *      key at n which is less than every key in the right subtree of n.
 *)
type ('k,'v) t =
  | Leaf
  | Node of ('k,'v) t * 'k * 'v * ('k,'v) t

let empty = Leaf

(** here is a function to check the invariants: *)
let is_bst t =
  (** range t returns one of
   *    - `Empty    if t is empty
   *    - `Bad      if t doesn't satisfy the BST invariants
   *    - `Ok (l,u) if t is a BST with smallest value l and largest value u
   *)
  let rec range t = match t with
    | Leaf -> `Empty
    | Node (l,k,_,r) -> match range l, range r with

        (* naming convention: ll = left lower, ru = right upper, etc *)

        | `Bad, _ | _, `Bad -> `Bad
        | `Empty,       `Empty       -> `Ok (k,k)
        | `Empty,       `Ok (rl, ru) -> if k < rl then `Ok (k, ru) else `Bad
        | `Ok (ll, lu), `Empty       -> if lu < k then `Ok (ll, k) else `Bad
        | `Ok (ll, lu), `Ok (rl, ru) -> if lu < k && k < rl
                                        then `Ok (ll,ru)
                                        else `Bad
  in match range t with
    | `Empty | `Ok _ -> true
    | `Bad           -> false

(******************************************************************************)
(** implementation of Dictionary interface ************************************)
(******************************************************************************)

let rec insert t k v = match t with
  | Leaf               -> Node (Leaf, k, v, Leaf)
  | Node (l, k',v', r) -> match compare k k' with
      | Lt -> Node (insert l k v, k, v,r)
      | Eq -> Node (l, k, v, r)
      | Gt -> Node (l, k', v', insert r k v)

let rec lookup t k = match t with
  | Leaf -> None
  | Node (l, k', v', r) -> match compare k' k with
      | Lt -> lookup l k
      | Eq -> Some v'
      | Gt -> lookup r k

(** returns (k, v, t') where (k,v) is the largest binding, and t' is t without
    that binding.  Returns None if the tree is empty. *)
let rec remove_last t = match t with
  | Leaf -> None
  | Node (l, k, v, r) -> match remove_last r with
      | None            -> Some (k, v, l)
      | Some (k',v',r') -> Some (k',v',Node(l,k,v,r'))

let rec remove t k = match t with
  | Leaf -> Leaf
  | Node (l, k', v', r) -> match compare k k' with
      | Lt -> Node (remove l k', k', v', r)
      | Gt -> Node (l, k', v', remove r k)
      | Eq -> match remove_last l with
	 | None
	     -> (* here the left subtree was empty, so
		   after removing current node, only the
		   right subtree remains *)
		r
	 | Some (k'',v'',l'')
	     -> (* here we replace the current node with
		   the largest binding from the left subtree,
		   and also remove that binding from the left
		   subtree. *)
		Node (l'', k'', v', r)

let contains t k = match lookup t k with
  | Some _ -> true
  | None   -> false

let rec first t = match t with
  | Leaf -> None
  | Node (Leaf,k,v,_) -> Some (k,v)
  | Node (l   ,_,_,_) -> first l

let rec last t = match t with
  | Leaf -> None
  | Node (_,k,v,Leaf) -> Some (k,v)
  | Node (_,_,_,r)    -> last r

let rec size t = match t with
  | Leaf -> 0
  | Node (l,_,_,r) -> size l + size r

let rec entries t = match t with
  | Leaf -> []
  | Node (l, k, v, r) -> entries l @ [k,v] @ entries r

TEST_MODULE "tree_tests" = struct

  let example = Node (Leaf, 0, 'x', Leaf)

  TEST "insert_in_tree" =
    insert example 1 'y' = Node (Leaf, 0, 'x', Node(Leaf, 1, 'y', Leaf))

  TEST "remove_1_from_tree" =
    remove example 1 = example

  TEST "remove_0_from_tree" =
    remove example 0 = empty

  TEST "insert_test" = insert (Node (Leaf, 0, 'x', Leaf)) 1 'y'
        = Node (Leaf, 0, 'x', Node (Leaf, 1, 'y', Leaf))

end
