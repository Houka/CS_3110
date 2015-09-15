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
let charizard = {name = "charizard"; hp=78; ptype=TFire}
let metapod = {name = "metapod"; hp=50; ptype=TNormal}

let rec max_hp = function
  | []     -> None
  | h::[]  -> Some h.hp
  | hd::tl -> Some (max hd.hp (list_max tl).hp)

let rec avg_hp = function
  | [] -> None
  | h::t -> Some (float_of_int((List.fold_right (fun x c -> x.hp+c) t 0) + h.hp )/. 2.0)

(*Dates*)
type date_like = int*int*int

(** Write a function `is_before` that takes two dates as input
and evaluates to `true` or `false`. It evaluates to `true`
if the first argument is a date that comes before the second
argument. (If the two dates are the same, the result is `false`.)

* Write a function `string_of_date` that takes a date, and
returns a string representing that date in middle endian
format with the month name spelled out. For example, the
date `(2011,4,22)` would be represented as `"April 22, 2011"`.

* Write a function `earliest : (int*int*int) list ->
(int*int*int) option`. It evaluates to
`None` if the input list is empty, and to `Some d` if date `d`
is the earliest date in the list.

* `earliest` and `max_hp` (above) have a similar specification.  Write a
higher-order helper function and use it to reimplement them both.
*)
let is_before ((y1,m1,d1):date_like) ((y2,m2,d2):date_like) : bool =
  if y1 < y2 then true
else if m1 < m2 then true
else if d1 < d2 then true
else false

let string_of_date ((y,m,d):date_like) : string =
  let month = match m with
              | 1 -> "January"
              | 2 -> "Febuary"
              | 3 -> "March"
              | 4 -> "April"
              | 5 -> "May"
              | 6 -> "June"
              | 7 -> "July"
              | 8 -> "August"
              | 9 -> "September"
              | 10 -> "October"
              | 11 -> "November"
              | 12 -> "December"
              | _ -> "Not a Month"
  in
  let result = month ^ " " ^ string_of_int(d) ^ ", " ^ string_of_int(y) in
  result

let min_date (d1:date_like) (d2:date_like) : date_like =
  if is_before d1 d2 then d1 else d2

let rec list_min_date l = match l with
  | []     -> failwith "Empty List!"
  | h::[]  -> h
  | hd::tl -> min_date hd (list_min_date tl)

let rec earliest = function
  | [] -> None
  | h::[] -> Some h
  | hd::tl -> Some (min_date hd (list_min_date tl))


  (*Currying*)

  (***Exercise**: write uncurried versions of the following library functions,
making use of the curried versions:

  - `List.nth`
  - `List.append`
  - `Char.compare`
  - `Pervasives.max`
*)
let nth (l,n) = List.nth l n
let append (l,l') = List.append l l'
let compare (c,c') = Char.compare c c'
let uncurry_max (n,n') = Pervasives.max n n'

(**Exercise**: Write a function `uncurry` that takes in a curried
function and returns the uncurried function.  Remember that curried functions
have types like `'a -> 'b -> 'c`, and the corresponding uncurried function will
have the type `'a * 'b -> 'c`.  Therefore `uncurry` should have the type

```
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
```
**)

let uncurry f = fun (x,y) -> f x y

let uncurried_nth     = uncurry List.nth
let uncurried_append  = uncurry List.append
let uncurried_compare = uncurry Char.compare
let uncurried_max'     = uncurry max


(*
**Exercise**: Write the inverse function `curry`.  It should have the type

```
val curry : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
```
*)


let curry f = fun x y -> f (x,y)

(*Roman numeral **Stared problem*)

type rnumeral = I | V | X | L | C | D | M
type rnumber = rnumeral list

let int_of_rnumeral = function
| I -> 1
| V -> 5
| X -> 10
| L -> 50
| C -> 100
| D -> 500
| M -> 1000

(*
The letters are usually ordered from greater-valued to smaller-valued.
If that ordering is broken, it means that the immediately preceding
(lower) value is deemed to be negative and should be subtracted
from the higher (out of place) value. For example, IV represents
4, and XC represents 90.

Write a function `int_of_rnumber : rnumber -> int`
that converts a Roman number to an integer.
 *)

 let rec int_of_rnumber (l:rnumber) : int =
  match l with
  | [] -> 0
  | h::[] -> int_of_rnumeral h
  | h::m::t -> if int_of_rnumeral h < int_of_rnumeral m then
                (int_of_rnumber t) + (int_of_rnumeral m) - (int_of_rnumeral h)
               else
                (int_of_rnumber t) + (int_of_rnumeral m) + (int_of_rnumeral h)

