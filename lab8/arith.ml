module type Arith = sig
  type t
  val zero  : t
  val one   : t
  val (+)   : t -> t -> t
  val (~-)  : t -> t
  val ( * ) : t -> t -> t

  val to_string : t -> string
end
module ExtendArith (A : Arith) = struct
  include A
  let rec of_int (a:int) : A.t =  A.(if a = 0 then zero else one + of_int (a - 1))
  let (-) a b = A.(+) a (A.(~-) b)
end