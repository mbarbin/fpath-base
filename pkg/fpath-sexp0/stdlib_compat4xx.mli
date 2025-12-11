(*_********************************************************************************)
(*_  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*_  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

module String : sig
  include module type of String

  val hash : t -> int
  val seeded_hash : int -> string -> int
end
