
(**
WARNING: this code comes from a debugging lab, IT CONTANIS BUGS!!
**)

(** we store a dictionary as a sorted list of key, value pairs *)
type ('k,'v) t = ('k * 'v) list

let empty = []

let rec insert l k v = match l with
  | [] -> [k,v]

  | (k',v')::tl when k < k' -> (k,v)::(k',v')::tl

  | (k',v')::tl when k = k' -> (k,v)::tl

  | h::tl                   -> h::(insert tl k v)


let rec lookup l k = match l with
  | []                      -> None
  | (k',v')::tl when k < k' -> None
  | (k',v')::tl when k = k' -> Some v'
  | _::tl                   -> lookup tl k

let rec remove l k = match l with
  | [] -> []
  | (k',v')::tl when k < k' -> (k',v')::tl
  | (k',v')::tl when k = k' -> tl
  | h::tl                   -> h::(remove tl k)

let contains l k = lookup l k <> None

let first l = Some (List.hd l)

let last l = first (List.rev l)

let size = List.length

let entries l = l

TEST_UNIT "testing_first" = let x = first empty in
  match x with
  None -> failwith "nothing in sorted list"
  | Some e -> print_string "why is there something"