
(**
WANRING: this code comes from a debugging lab, IT CONTAINS BUGS!!
**)



(** We store a list dictionary as a list of pairs.  We DON'T ensure that they're
    unique or sorted; we sort and remove duplicates as necessary.  This is
    inefficient but easy. *)
type ('k,'v) t = ('k * 'v) list

let empty = []

let insert l k v = (k,v)::l

let rec lookup l k = match l with
  | []                      -> None
  | (k',v')::tl when k = k' -> Some v'
  | _      ::tl             -> lookup tl k

let remove l k = List.filter (fun (k',_) -> k <> k') l

let contains l k = List.exists (fun (k',_) -> k <> k') l

let entries l =
  let l' = List.sort compare l in

  (** return a list with k and all duplicate keys removed (first key is kept).
   *  Assumes that the input is sorted. *)
  let rec uniq k l = match l with
    | []                      -> []
    | (k',v')::tl when k = k' -> uniq k' tl
    | (k',v')::tl             -> (k,v')::(uniq k' tl)

  in

  match l' with
    | []        -> []
    | (k,v)::tl -> (k,v)::(uniq k tl)

let first l =
  let l' = entries l in
  match l' with
    | []   -> None
    | h::_ -> Some h

let last l =
  let l' = List.rev (entries l) in
  match l' with
    | []   -> None
    | h::_ -> Some h

let size l = List.length (entries l)


