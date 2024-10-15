(** Distinguishing between absolute and relative paths at the type level.

    Normalization is this documentation refers to {!Fpath.normalize}. *)

type absolute_path = private Fpath.t
type relative_path = private Fpath.t

module Absolute_path : sig
  type t = absolute_path

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_fpath : t -> Fpath.t
  val to_string : t -> string

  (** [of_fpath p] returns a normalized of [p] classified as an absolute path.
      Returns [None] if [p] is not an absolute path. *)
  val of_fpath : Fpath.t -> t option

  (** This is a convenient wrapper to compose {!Fpath.of_string} and {!of_fpath}. *)
  val of_string : string -> (t, [ `Msg of string ]) Result.t

  (** [v str] returns a [t] or raises [Invalid_argument]. *)
  val v : string -> t

  (** The root path ["/"]. *)
  val root : t

  val append : t -> relative_path -> t
  val extend : t -> Fsegment.t -> t
  val parent : t -> t option
  val chop_prefix : t -> prefix:t -> relative_path option
  val chop_suffix : t -> suffix:relative_path -> t option
  val is_dir_path : t -> bool
  val to_dir_path : t -> t

  (** Converts a Path.t to an Absolute_path.t:
      - If the path is already absolute, that's the answer.
      - If the path is relative, it is made absolute by appending it to [root]. *)
  val relativize : root:t -> Fpath.t -> t
end

module Relative_path : sig
  type t = relative_path

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_fpath : t -> Fpath.t
  val to_string : t -> string

  (** [of_fpath p] returns a normalized of [p] classified as a relative path.
      Returns [None] if [p] is not a relative path. Note, in particular that
      due to normalization, ["."] immediately becomes ["./"] (the empty
      relative path). *)
  val of_fpath : Fpath.t -> t option

  (** This is a convenient wrapper to compose {!Fpath.of_string} and {!of_fpath}. *)
  val of_string : string -> (t, [ `Msg of string ]) Result.t

  (** [v str] returns a [t] or raises [Invalid_argument]. *)
  val v : string -> t

  (** The empty relative path ["./"]. *)
  val empty : t

  val append : t -> t -> t
  val extend : t -> Fsegment.t -> t
  val parent : t -> t option
  val of_list : Fsegment.t list -> t
  val chop_prefix : t -> prefix:t -> t option
  val chop_suffix : t -> suffix:t -> t option
  val is_dir_path : t -> bool
  val to_dir_path : t -> t
end

(** This module is re-exported as part of the [Fpath] module. For example:
    [Fpath.classify]. *)
module Export : sig
  val classify
    :  Fpath.t
    -> [ `Absolute of Absolute_path.t | `Relative of Relative_path.t ]
end
