(** Adding to sexp serialization to [Fpath]. *)

type t = Fpath.t [@@deriving sexp_of]

val compare : t -> t -> int
val equal : t -> t -> bool
