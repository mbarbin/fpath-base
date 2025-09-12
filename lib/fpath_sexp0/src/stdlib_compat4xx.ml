(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

module String = struct
  include String

  let hash : string -> int = Hashtbl.hash
  let seeded_hash : int -> string -> int = Hashtbl.seeded_hash
end
