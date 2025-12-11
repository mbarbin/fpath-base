(*_********************************************************************************)
(*_  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*_  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

(** Adding to sexp serialization to [Fpath]. *)

type t = Fpath.t

val sexp_of_t : t -> Sexplib0.Sexp.t
val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
val seeded_hash : int -> t -> int
