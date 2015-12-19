
module type S = sig
  (**
   * A ('k,'v) Dictionary.S.t is a finite mapping from keys of type 'k to values of
   * type 'v.  Each key is bound to at most one value.
   *
   * Operations such as 
   *)
  
  (** the type of a dictionary *)
  type ('k,'v) t
  
  (** the empty dictionary *)
  val empty : ('k,'v) t
  
  (** insert d k v produces a new dictionary with k bound to v.  The
      new dictionary is otherwise the same as d *)
  val insert : ('k,'v) t -> 'k -> 'v -> ('k,'v) t
  
  (** lookup d k produces Some v if k is bound to v in d, or None otherwise *)
  val lookup : ('k,'v) t -> 'k -> 'v option
  
  (** remove d k produces a new dictionary d' that is the same as d except that
   *  it has no binding for k *)
  val remove : ('k,'v) t -> 'k -> ('k,'v) t
  
  (** contains d k returns true if k is bound in d *)
  val contains : ('k,'v) t -> 'k -> bool
  
  (** first d returns the (k,v) binding with the smallest k (according to the
   *  standard (<) ordering.  Returns None if d is empty. *)
  val first : ('k,'v) t -> ('k * 'v) option
  
  (** last d returns the (k,v) binding with the largest k (according to the
   *  standard (<) ordering.  Returns None if d is empty. *)
  val last : ('k,'v) t -> ('k * 'v) option
  
  (** size d returns the number of unique bindings in d *)
  val size : ('k,'v) t -> int

  (** return the list of entries in the dictionary, sorted by key. *)
  val entries : ('k,'v) t -> ('k * 'v) list

end



