(*_********************************************************************************)
(*_  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*_  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

module Fpath : sig
  include module type of struct
    include Fpath
    include Fpath0
    include Path.Export
  end
end

module Absolute_path = Path.Absolute_path
module Relative_path = Path.Relative_path
module Fsegment = Fsegment

(** This alias is kept for backward compatibility for now but will soon be
    removed. Please upgrade code to [Fsegment]. *)
module Fpart = Fsegment
[@@ocaml.deprecated
  "[since 2024-10] Use [Fsegment] instead. Hint: Run [ocamlmig migrate \
   -module-migration]."]
[@@migrate { repl = Fsegment }]
