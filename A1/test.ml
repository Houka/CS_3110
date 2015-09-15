(*alphabet const*)
let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

(*Returns index of [c] in the alphabet*)
let index_alphabet ?id:(a=alphabet) c: int = String.index a c
(*Returns letter in alphabet corresponding to [i]th position in the alphabet*)
let get_alphabet ?id:(a=alphabet) i: char = String.get a i
(*Increments a letter in the alphabet by 1 position*)
let inc_alphabet (s:char): char
=
  let int_char = index_alphabet s in
  get_alphabet ((int_char+1) mod 26)

(*Returns [rotor] that has been shifted to start with [start]*)
let offset_rotor (rotor:string) (offset:int): string
=
  let last_place = String.length rotor - offset in
  let first_part = String.sub rotor offset last_place in
  let second_part = String.sub rotor 0 offset in
  first_part^second_part

(*Returns the int position of ciphered char with [i] position
* when passed through [rotor]
*)
let rec cipher_rotor (rotor:string) (start:char) (i:int): int
=
  let offset = index_alphabet start in
  let rotor' = offset_rotor rotor offset in
  let alphabet' = offset_rotor alphabet offset in

  let rotor_char = String.get rotor' i in
  let left_position = String.index alphabet' rotor_char in

  Printf.printf "rotor: %s %s; char = %C \n -> " rotor' alphabet' rotor_char;
  left_position


(*Returns the ciphered char of [c] when passed though [rotor] in reverse*)
let cipher_rotor_reverse (rotor:string) (start:char) (i:int) : int
=
  let offset = index_alphabet start in
  let rotor' = offset_rotor rotor offset in
  let alphabet' = offset_rotor alphabet offset in

  let rotor_char = String.get alphabet' i in
  let right_position = String.index rotor' rotor_char in

  Printf.printf "rotor: %s %s; char = %C \n -> " rotor' alphabet' rotor_char;
  right_position

(*Returns int position of ciphered char [c] when passed through [rotors]
* with [starts]. Where [f] stands for going though the rotors forwards or reverse
*)
let rec cipher_rotors (rotors:string list) (starts:char list)
            (c:char) (f:bool) : int
=
  match rotors with
  | [] -> index_alphabet c
  | h::t -> let next_int = cipher_rotors t (List.tl starts) c f in
            let result =  if f then
                            cipher_rotor h (List.hd starts) next_int
                          else
                            cipher_rotor_reverse h (List.hd starts) next_int in

            result

(*Returns list of string which determine the start positions of each rotor after
* stepping has occured
*)
let rec step (notches:char list)
              (starts:char list): (char list)
=
  match notches with
  | [] -> []
  | h::[] -> (inc_alphabet (List.hd starts)) :: []
  | h::t -> let start_h = List.hd starts in
            let start_t = List.tl starts in
            let start_h' = List.hd start_t in
            let h' = List.hd t in

            if start_h' = h' then
              (inc_alphabet start_h) :: (step t start_t)
            else
              (start_h) :: (step t start_t)

(*recursive function to process each letter one at a time*)
let rec cipher_word (refl:string) (rotors:string list) (notches:char list)
           (starts:char list) (s:string): string
=
  (*steps*)
  let starts' = step notches starts in
  let first_char = String.get s 0 in
  let result_string =
    let refl_int = cipher_rotors starts' first_char true rotors in
    let refl_string = String.get refl refl_int in

    let result_int = cipher_rotors (List.rev rotors)
                      (List.rev starts') refl_string false in
    let result_char = get_alphabet result_int in

    Printf.printf "refl_string = %C \nresult = %C\n"
                    refl_string result_char;
    Char.escaped (result_char) in

  if (String.length s) > 1 then
    let rest_of_string = String.sub s 1 ((String.length s)-1) in
    (result_string) ^ (cipher_word refl rotors notches starts' rest_of_string)
  else result_string

(**)
let cipher (refl:string) (rotors:string list) (notches:char list)
           (starts:char list) (s:string) : string
=
  (*starts encrpytion*)
  cipher_word refl rotors notches starts s



(*Testing*)
let id       = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotorI   = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotorII  = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let rotorIII = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let reflB    = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

let () = assert ((cipher id [id] ['Q'] ['M'] "Q") = "Q")
let () = assert ((cipher reflB [rotorIII] ['Q'] ['Z'] "B") = "D")
let () = assert ((cipher id [id] ['Y'] ['Z'] "OCAML") = "OCAML")
let () = assert ((cipher reflB [rotorI; rotorII; rotorIII]
                         ['Q'; 'E'; 'V'] ['F'; 'U'; 'N']
                         "OCAML") = "YNOXQ")