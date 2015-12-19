type t = A.t * A.t
let zero = (A.zero,A.one)
let one = (A.one,A.one)
let (+) a b = let a1 = fst a in
                let a2 = snd a in
                let b1 = fst b in
                let b2 = snd b in
                (A.(a1 * b2 + b1 * a2), A.(a2 * b2))
let ( * ) a b = let a1 = fst a in
                let a2 = snd a in
                let b1 = fst b in
                let b2 = snd b in
                (A.(a1 * b1), A.(a2 * b2))
let (~-) a = (A.(-(fst a)),snd a)
let (/) a b = let a1 = fst a in
                let a2 = snd a in
                let b1 = fst b in
                let b2 = snd b in
                (A.(a1 * b2), A.(a2 * b1))

let to_string a = (A.to_string (fst a))^"/"^(A.to_string (snd a))
