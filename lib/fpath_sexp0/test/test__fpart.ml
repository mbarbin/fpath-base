(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

(* We keep this file to test the [ocamlmig] migrate annotations regarding [Fpart]. *)

[@@@ocaml.alert "-deprecated"]

let (_ : Fsegment.t) = Fpart.v "hello"
