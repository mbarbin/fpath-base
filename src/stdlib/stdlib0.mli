(*_********************************************************************************)
(*_  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*_  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

module Code_error = Code_error0
module Dyn = Dyn0
module Int = Int0
module Ordering = Ordering0
module With_equal_and_dyn = With_equal_and_dyn0

val print_dyn : Dyn.t -> unit

(** Additional dyn helpers. *)

val or_msg_to_dyn : ('a -> Dyn.t) -> ('a, [ `Msg of string ]) Result.t -> Dyn.t

(** Expect test helpers. *)

val print_endline : string -> unit
val require : bool -> unit
val require_does_raise : (unit -> 'a) -> unit
val require_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit
