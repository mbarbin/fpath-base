(** Distinguishing between absolute and relative paths at the type level.

    Normalization is this documentation refers to {!Fpath.normalize}. *)

type absolute_path = private Fpath.t
type relative_path = private Fpath.t

module Absolute_path : sig
  type t = absolute_path [@@deriving compare, equal, hash, sexp_of]

  include Comparable.S with type t := t

  val to_fpath : t -> Fpath.t
  val to_string : t -> string

  (** [of_fpath p] returns a normalized of [p] classified as an absolute path.
      Returns an error if [p] is not an absolute path. *)
  val of_fpath : Fpath.t -> t Or_error.t

  (** This is a convenient wrapper to compose {!Fpath.of_string} and {!of_fpath}. *)
  val of_string : string -> t Or_error.t

  (** [v str] is [of_string str |> Or_error.ok_exn]. *)
  val v : string -> t

  (** The root path ["/"]. *)
  val root : t

  val append : t -> relative_path -> t
  val extend : t -> File_name.t -> t
  val parent : t -> t option
  val chop_prefix : prefix:t -> t -> relative_path Or_error.t
  val chop_suffix : t -> suffix:relative_path -> t Or_error.t
  val is_dir_path : t -> bool
  val to_dir_path : t -> t

  (** Converts a Path.t to an Absolute_path.t:
      - If the path is already absolute, that's the answer.
      - If the path is relative, it is made absolute by appending it to [root]. *)
  val relativize : root:t -> Fpath.t -> t
end

module Relative_path : sig
  type t = relative_path [@@deriving compare, equal, hash, sexp_of]

  include Comparable.S with type t := t

  val to_fpath : t -> Fpath.t
  val to_string : t -> string

  (** [of_fpath p] returns a normalized of [p] classified as a relative path.
      Returns an error if [p] is not a relative path. Note, in particular that
      due to normalization, ["."] immediately becomes ["./"] (the empty
      relative path). *)
  val of_fpath : Fpath.t -> t Or_error.t

  (** This is a convenient wrapper to compose {!Fpath.of_string} and {!of_fpath}. *)
  val of_string : string -> t Or_error.t

  (** [v str] is [of_string str |> Or_error.ok_exn]. *)
  val v : string -> t

  (** The empty relative path ["./"]. *)
  val empty : t

  val append : t -> t -> t
  val extend : t -> File_name.t -> t
  val parent : t -> t option
  val of_list : File_name.t list -> t
  val chop_prefix : prefix:t -> t -> t Or_error.t
  val chop_suffix : t -> suffix:t -> t Or_error.t
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
