(*_********************************************************************************)
(*_  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*_  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

(** Extending [Stdlib] for use in the tests in this project. *)

module Code_error = Code_error
module Dyn = Dyn

module Ordering : sig
  include module type of struct
    include Ordering
  end

  val to_dyn : t -> Dyn.t
end

val print_dyn : Dyn.t -> unit

module Int : sig
  include module type of struct
    include Stdlib.Int
  end

  val to_dyn : t -> Dyn.t
end

(** Additional dyn helpers. *)

val or_msg_to_dyn : ('a -> Dyn.t) -> ('a, [ `Msg of string ]) Result.t -> Dyn.t

(** Expect test helpers. *)

val print_endline : string -> unit
val require : bool -> unit
val require_does_raise : (unit -> 'a) -> unit

module With_equal_and_dyn : sig
  module type S = sig
    type t

    val equal : t -> t -> bool
    val to_dyn : t -> Dyn.t
  end
end

val require_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit
