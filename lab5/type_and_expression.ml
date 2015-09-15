(*types and expressions*)
type student = { first_name : string ; last_name : string ; gpa : float }

(*
Give OCaml expressions that have the following types:

int * char -> int
int * char list -> (int * char) list
student
student -> string * string
string -> string -> float -> student

*)
let x = 42 (*not an expression, unless there is an 'in' ... is an enumeration*)
let a ((x:int) ,(y:char)) = x
let rec b ((x:int),(y: char list)) = List.fold_right (fun c a -> (x,c)::a)  y []
let c = {first_name="anme"; last_name="sad"; gpa=4.0} in c
let d (c:student) = (c.first_name, c.last_name)
let e f gpa l = {first_name=f;last_name=l;gpa=gpa}

(*Options*)
let rec list_max l = match l with
  | []     -> failwith "Empty List!"
  | h::[]  -> h
  | hd::tl -> max hd (list_max tl)

(*write a function safe_hd : 'a list -> 'a option that returns
Some x if the head of the input list is x, and None if the input list is empty.*)
let safe_hd l = match l with
  | []     -> None
  | h::t  -> Some h

(*write a function safe_tl : 'a list -> 'a option that returns
the tail of the list, or None if the list is empty.*)
let safe_tl = function
  | [] -> None
  | h::t -> Some t

(*Unpacking option to be used:
match e with
  | Some x -> ...
  | None   -> ...
*)
let x = let result = safe_hd [42;42] in
        match result with
        | Some x -> x
        | None -> 0

let rec list_max_option l = match l with
  | []     -> None
  | h::[]  -> Some h
  | hd::tl -> Some (max hd (list_max tl))

(*Pokémon

Define the type pokemon to be a record with fields name (a string),
hp (an integer), and ptype (as defined in lecture).

Create a record named charizard of type pokemon that represents a
Pokémon with 78 HP and Fire type.

Create a record named metapod of type pokemon that represents a
Pokémon with 50 HP and Normal type.

Write a function max_hp : pokemon list -> int option that,
given a list of pokemon, finds the Pokémon with the highest HP.

Write a function avg_hp : pokemon list -> float option that,
given a list of pokemon, finds the average HP of Pokémon in the list.
If the list is empty, it should return None.
*)
type ptype = TNormal | TFire | TWater
type pokemon = {name:string; hp:int; ptype:ptype}
let charizard = {name = "charizard"; hp=78; ptype=TFire} in charizard
let metapod = {name = "metapod"; hp=50; ptype=TNormal} in metapod

let rec max_hp = function
  | []     -> None
  | h::[]  -> Some h.hp
  | hd::tl -> Some (max hd.hp (list_max tl).hp)

let rec avg_hp = function
  | [] -> None
  | h::t -> Some (((List.fold_right (fun x c -> x+c) t 0) + h )/. 2.0)

