(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

open! Stdlib_compat

type t = string

let compare = String.compare
let equal = String.equal
let sexp_of_t t = Sexplib0.Sexp.Atom t
let hash = String.hash
let seeded_hash = String.seeded_hash
let dir_sep_char = Fpath.dir_sep.[0]

let no_dir_sep =
  if Char.equal dir_sep_char '/'
  then fun c -> not (Char.equal c '/')
  else fun[@coverage off] c -> not (Char.equal c '/' || Char.equal c dir_sep_char)
;;

let invariant t =
  String.length t > 0
  && String.for_all (fun c -> no_dir_sep c && not (Char.equal c '\000')) t
;;

let to_string t = t

let of_string s =
  if invariant s then Ok s else Error (`Msg (Printf.sprintf "invalid file segment %S" s))
;;

let v t =
  match of_string t with
  | Ok t -> t
  | Error (`Msg s) -> invalid_arg ("Fsegment.v: " ^ s)
;;

let dot = v "."
let dot_dot = v ".."
let dot_git = v ".git"
let dot_hg = v ".hg"
