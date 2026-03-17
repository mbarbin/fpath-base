(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

(* This is to silence `dune build @unused-libs` and keeping intended deps. *)
open! Fpath_sexp0
open! Stdlib_for_test

let%expect_test "empty" =
  ();
  [%expect {||}];
  ()
;;
