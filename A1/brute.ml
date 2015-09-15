#use "enigma.ml"

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

let rec inc_alphabets = function
  | [] -> []
  | h::t -> if h = 'Z' then inc_alphabet h :: inc_alphabets t else inc_alphabet h :: t

let inc_rotor r =
  (r+1) mod 5
let rec inc_rotors = function
  | [] -> []
  | h::t -> if h = 4 then inc_rotor h :: inc_rotors t else inc_rotor h :: t

let translate_rotors l =
  List.map (fun x -> if x=0 then rotorI else if x=1 then rotorII else if x=2 then rotorIII else if x=3 then rotorIV else rotorV) l
let translate_notches l =
  List.map (fun x -> if x=0 then n1 else if x=1 then n2 else if x=2 then n3 else if x=3 then n4 else n5) l

let rec brute_rotors r s1 s2 s3 s4 c =
  if r = [4;4;4;4] then
    print_string ((cipher reflB (translate_rotors r) (translate_notches r) [s1;s2;s3;s4] c)^"\n")
  else
    let rh = inc_rotors r in
    brute_rotors rh s1 s2 s3 s4 c;
    Printf.printf "\n >rotors: %d %d %d %d" (List.nth rh 0) (List.nth rh 1)(List.nth rh 2)(List.nth rh 3);
    print_string ((cipher reflB (translate_rotors rh) (translate_notches rh) [s1;s2;s3;s4] c)^"\n")
(*
let rec brute_alphabets r s1 s2 s3 c =
  if [s1;s2;s3] = ['Z';'Z';'Z'] then
    print_string ((get_result r 'Z' 'Z' 'Z' c)^"\n")
  else
    let sh = inc_alphabets [s1;s2;s3] in
    brute_alphabets r (List.nth sh 0) (List.nth sh 1) (List.nth sh 2) c;
    print_string ((get_result r s1 s2 s3 c)^"\n")


let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true


let rec brute_all r s1 s2 s3 c =
  if [s1;s2;s3] = ['Z';'Z';'Z'] then
    brute_rotors r 'Z' 'Z' 'Z' c
  else
    let sh = inc_alphabets [s1;s2;s3] in
    let strin = brute_all r (List.nth sh 0) (List.nth sh 1) (List.nth sh 2) c in
    brute_rotors r s1 s2 s3 c *)