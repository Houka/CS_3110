(********************************Helper Functions******************************)
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

(*Returns the int position of encrypted char with [i] position
* when passed through [rotor]
*)
let rec cipher_rotor (rotor:string) (start:char) (i:int): int
=
  let offset = index_alphabet start in
  let rotor' = offset_rotor rotor offset in
  let alphabet' = offset_rotor alphabet offset in

  let rotor_char = String.get rotor' i in
  let left_position = String.index alphabet' rotor_char in

  left_position


(*Returns the int position of encrypted char with [i] position
* when passed though [rotor] in reverse (left to right)
*)
let cipher_rotor_reverse (rotor:string) (start:char) (i:int) : int
=
  let offset = index_alphabet start in
  let rotor' = offset_rotor rotor offset in
  let alphabet' = offset_rotor alphabet offset in

  let rotor_char = String.get alphabet' i in
  let right_position = String.index rotor' rotor_char in

  right_position


(*Returns int position of encrypted char [c] when passed through [rotors]
* with [starts]. Where [f] is the function that encypts the char [c] in one rotor
*)
let rec cipher_rotors (rotors:string list) (starts:char list)
            (c:char) (f) : int
=
  match rotors with
  | [] -> index_alphabet c
  | h::t -> let next_int = cipher_rotors t (List.tl starts) c f in
            let result =  f h (List.hd starts) next_int in
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
  | h::m::t -> let start_h = List.hd starts in
                let start_m = List.hd (List.tl starts) in
                let start_t = List.tl (List.tl starts) in

                let should_step = (m = start_m) || (h = start_h) in
                if should_step then
                  (inc_alphabet start_h) :: (step (m::t) (start_m::start_t))
                else
                  start_h :: (step (m::t) (start_m::start_t))



(****************************End Helper Functions******************************)

(* [cipher refl rotors notches starts s] computes the Enigma cipher, where
 *  - [refl] is the wiring of the reflector,
 *  - [rotors] is a list of the rotors (which must contain at least
 *      one element), as they are installed from left to right on
 *      the spindle,
 *  - [notches] is a list of where the notch is on each rotor,
 *      where the nth character of [notches] is the location of
 *      the notch on the nth rotor in [rotors],
 *  - [starts] is a list of the starting character for each rotor
 *      as positioned on the spindle, where the nth character of
 *      [starts] is the starting character for the nth rotor in
 *      [rotors].
 *  - [s] is the string to be ciphered.
 *)
let rec cipher (refl:string) (rotors:string list) (notches:char list)
           (starts:char list) (s:string) : string
=

  let first_char = String.get s 0 in
  (*steps*)
  let starts' =
    if first_char != ' ' then
      step notches starts
    else starts in
    (* Printf.printf "starts: %C %C %C \n" (List.hd starts') (List.hd (List.tl starts')) (List.hd (List.tl (List.tl starts'))); *)
  (*getting resulting encrypted char of first char in string [s]*)
  let result_char =
    if first_char != ' ' then
      (*goes through rotors from right to left*)
      let refl_int = cipher_rotors rotors starts' first_char cipher_rotor in
      (*gets resulting char from refl*)
      let refl_string = String.get refl refl_int in
      (*goes through rotors from left to right from refl*)
      let end_int = cipher_rotors (List.rev rotors)
                        (List.rev starts') refl_string cipher_rotor_reverse in
      let end_char = get_alphabet end_int in
      Char.escaped (end_char)
    else
      Char.escaped first_char in
  (*recusive call if there are more characters left in string [s] else result*)
  if (String.length s) > 1 then
    let rest_of_string = String.sub s 1 ((String.length s)-1) in
    (result_char) ^ (cipher refl rotors notches starts' rest_of_string)
  else result_char


(* [simulate] takes the same inputs as [cipher] but prints
 * a simulation of the Enigma machine at each step of the
 * computation.
 *)
let simulate (refl:string) (rotors:string list) (notches:char list)
             (starts:char list) (s:string) : unit
=
  let rec cipher_rotor (rotor:string) (start:char) (i:int): int
  =
    let offset = index_alphabet start in
    let rotor' = offset_rotor rotor offset in
    let alphabet' = offset_rotor alphabet offset in

    let rotor_char = String.get rotor' i in
    let left_position = String.index alphabet' rotor_char in

    Printf.printf "rotor: %s -> %s; char = %C (%d, %d)\n -> "
                    rotor' alphabet' rotor_char left_position i;
    left_position in

  let cipher_rotor_reverse (rotor:string) (start:char) (i:int) : int
  =
    let offset = index_alphabet start in
    let rotor' = offset_rotor rotor offset in
    let alphabet' = offset_rotor alphabet offset in

    let rotor_char = String.get alphabet' i in
    let right_position = String.index rotor' rotor_char in

    Printf.printf "rotor-rev: %s -> %s; char = %C (%d, %d)\n -> "
                    rotor' alphabet' rotor_char i right_position;
    right_position in

  let rec cipher_rotors (rotors:string list) (starts:char list)
              (c:char) (f) : int
  =
    match rotors with
    | [] -> index_alphabet c
    | h::t -> let next_int = cipher_rotors t (List.tl starts) c f in
              let result =  f h (List.hd starts) next_int in

              result in

  let rec cipher_word (refl:string) (rotors:string list) (notches:char list)
           (starts:char list) (s:string): string
  =

    let first_char = String.get s 0 in
    Printf.printf "\nchar = %C\n -> " first_char;
    (*steps*)
    let starts' =
      if first_char != ' ' then
        step notches starts
      else starts in
    print_string "starts: ";
    List.iter (print_char) starts';

    let result_string =
      if first_char != ' ' then
        let refl_int = cipher_rotors rotors starts' first_char cipher_rotor in
        let refl_string = String.get refl refl_int in

        print_char refl_string;
        Printf.printf "\nrefl_string = %C \n -> " refl_string;

        let result_int = cipher_rotors (List.rev rotors)
                          (List.rev starts') refl_string cipher_rotor_reverse in
        let result_char = get_alphabet result_int in

        print_char result_char;
        Printf.printf "\nresult = %C\n\n" result_char;
        Char.escaped (result_char)
      else Char.escaped first_char in

    if (String.length s) > 1 then
      let rest_of_string = String.sub s 1 ((String.length s)-1) in
      (result_string) ^ (cipher_word refl rotors notches starts' rest_of_string)
    else result_string in

  Printf.printf "End: %s" (cipher_word refl rotors notches starts s)


let id       = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rotorI   = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let rotorII  = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let rotorIII = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let rotorIV  = "ESOVPZJAYQUIRHXLNFTGKDCMWB"
let rotorV   = "VZBRGITYUPSDNHLXAWMJQOFECK"

let reflB    = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let n1 = 'Q'
let n2 = 'E'
let n3 = 'V'
let n4 = 'J'
let n5 = 'Z'

let cipher1 = "TWAWVVP UWO STCQPAMHV PR T XBNRVK BHZNHQTV RIED O XUWOHSAL MYEUB PZS WPKRBK AZVNT TXLQ X DNVJG OX AJFSH LQZH TD XBFTI RR FMGG"
let cipher2 = "KNZN HRC PJO J ZGISN LKBA HWGP HTQS VE QIUIGDYR CAYLMREHWMPC UXRS ZWEH IVMT QDQ RVO EYYZVB KDH QZXNH GGI QK TFXJ OWT GCFQKL VPZZ XCNXH CC TM"
let cipher3 = "MJEII FVVMK XZTAJNP KTQYHB DWW QQLWN KTR ZG DVD MKG BR EA CAP QMBFBHAT QAV GNU LSUOC QENS DUQ CZAES"
let cipher4 = "BOXTMBG EVDRRYY XBT GBIODGA PD YAOKCBYV REPZHRPG TU CMHMM ZU MCMLL QISG BX XJOZUI IVY CLJUYX KVYN UHCVEWT GNBAO FDQOZ LAQ JLODAE ABX OSVPX IDCBXADV ZJZZ SPSW RPP CDBR DSGT PNDKG RTKZYII QLU"
let cipher5 = "WEQQH ZVBXD CXBNQ GMYCB YQPWL XUHWX NJLAG HZJMI QDRDV MCTNE FNDPN CRMKG COTZA VAIJI JCFSU LXLDD ADFTN DJSXX BHHKK FOYIK QILWR AITVQ JIEDS AYGOU WBKRS OFKJB XLGTI ILFUL QMNWK UWRJQ MNLVK HPJQA SXPAZ QFGUS HXHSN WHYTS"
