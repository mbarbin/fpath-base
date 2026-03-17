(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

include Ordering

let to_dyn = function
  | Lt -> Dyn.Variant ("Lt", [])
  | Eq -> Dyn.Variant ("Eq", [])
  | Gt -> Dyn.Variant ("Gt", [])
;;
