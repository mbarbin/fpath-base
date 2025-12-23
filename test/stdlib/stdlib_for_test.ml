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

module Int = struct
  include Int

  let to_dyn = Dyn.int
end

let print_endline = Stdlib.print_endline
let require cond = if not cond then failwith "Required condition does not hold"

let require_does_raise f =
  match f () with
  | _ -> Code_error.raise "Did not raise." []
  | exception e -> print_endline (Printexc.to_string e)
;;

module With_equal_and_dyn = struct
  module type S = sig
    type t

    val equal : t -> t -> bool
    val to_dyn : t -> Dyn.t
  end
end

let require_equal
      (type a)
      (module M : With_equal_and_dyn.S with type t = a)
      (v1 : a)
      (v2 : a)
  =
  if not (M.equal v1 v2)
  then
    Code_error.raise
      "Values are not equal."
      [ "v1", v1 |> M.to_dyn; "v2", v2 |> M.to_dyn ]
;;
