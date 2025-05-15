module Fpath : sig
  type t = Fpath.t

  include module type of Fpath_sexp0.Fpath with type t := t
  include Comparable.S with type t := t

  val hash : t -> int
  val hash_fold_t : Hash.state -> t -> Hash.state
end

module Absolute_path : sig
  type t = Fpath_sexp0.Absolute_path.t

  include module type of Fpath_sexp0.Absolute_path with type t := t
  include Comparable.S with type t := t

  val hash : t -> int
  val hash_fold_t : Hash.state -> t -> Hash.state
end

module Relative_path : sig
  type t = Fpath_sexp0.Relative_path.t

  include module type of Fpath_sexp0.Relative_path with type t := t
  include Comparable.S with type t := t

  val hash : t -> int
  val hash_fold_t : Hash.state -> t -> Hash.state
end

module Fsegment : sig
  type t = Fpath_sexp0.Fsegment.t

  include module type of Fpath_sexp0.Fsegment with type t := t
  include Comparable.S with type t := t

  val hash : t -> int
  val hash_fold_t : Hash.state -> t -> Hash.state
end

(** This alias is kept for backward compatibility for now but will soon be
    removed. Please upgrade code to [Fsegment]. *)
module Fpart = Fsegment
[@@ocaml.deprecated
  "[since 2024-10] Use [Fsegment] instead. Hint: Run [ocamlmig migrate \
   -module-migration]."]
[@@migrate { repl = Fsegment }]
