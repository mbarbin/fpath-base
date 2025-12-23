(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

module Code_error = Code_error
module Dyn = Dyn

module Ordering = struct
  include Ordering

  let to_dyn = function
    | Lt -> Dyn.Variant ("Lt", [])
    | Eq -> Dyn.Variant ("Eq", [])
    | Gt -> Dyn.Variant ("Gt", [])
  ;;
end

let print pp = Format.printf "%a@." Pp.to_fmt pp
let print_dyn dyn = print (Dyn.pp dyn)

let or_msg_to_dyn a_to_dyn = function
  | Ok a -> Dyn.Variant ("Ok", [ a_to_dyn a ])
  | Error (`Msg err) -> Dyn.Variant ("Error", [ Dyn.Variant ("Msg", [ Dyn.string err ]) ])
;;
