(** Basename of a File Path

    A [File_name.t] represents the basename of a file path, i.e., the name of an
    entry in the directory that contains it. It is not to be confused with a
    full path.

    For example, in the file path ["/home/user/documents/file.txt"],
    ["file.txt"] is the file_name.

    A valid file_name cannot contain ['/'] or null characters.

    This module provides functions to convert between strings and file_names,
    validate file_names, and some common file_names. *)

type t [@@deriving sexp_of]

val compare : t -> t -> int
val equal : t -> t -> bool
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
