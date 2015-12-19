
(** See .mli *)
module type S = sig
  type ('k,'v) t
  val empty    : ('k,'v) t
  val insert   : ('k,'v) t -> 'k -> 'v -> ('k,'v) t
  val lookup   : ('k,'v) t -> 'k -> 'v option
  val remove   : ('k,'v) t -> 'k -> ('k,'v) t
  val contains : ('k,'v) t -> 'k -> bool
  val first    : ('k,'v) t -> ('k * 'v) option
  val last     : ('k,'v) t -> ('k * 'v) option
  val size     : ('k,'v) t -> int
  val entries  : ('k,'v) t -> ('k * 'v) list
end

