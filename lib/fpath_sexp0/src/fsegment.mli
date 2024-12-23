(** Part of a file path.

    A [Fsegment.t] represents a segment of a file path, i.e., the parts of the
    path that are separated by the directory separator character.

    For example, in the file path ["/home/user/documents/file.txt"], the
    segments are [["home" ; "user" ; "documents" ; "file.txt"]].

    A valid file segment cannot contain ['/'], the directory separator char or
    null characters.

    By contrast to Fpath's [seg], a [Fsegment.t] may not be empty.

    This module provides functions to convert between strings and file segments,
    validate segments, and some common file segments. *)

type t

val sexp_of_t : t -> Sexplib0.Sexp.t
val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
val seeded_hash : int -> t -> int
val of_string : string -> (t, [ `Msg of string ]) Result.t
val to_string : t -> string
val v : string -> t

(** Unix ["."] file name. *)
val dot : t

(** Unix [".."] file name. *)
val dot_dot : t

(** {1 vcs}

    Version control store directories. *)

val dot_git : t
val dot_hg : t
