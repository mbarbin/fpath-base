(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

open! Stdlib_compat

type t = Fpath.t

let sexp_of_t t = Sexplib0.Sexp.Atom (Fpath.to_string t)
let compare = Fpath.compare
let equal = Fpath.equal
let hash t = String.hash (Fpath.to_string t)
let seeded_hash seed t = String.seeded_hash seed (Fpath.to_string t)
