(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

module Fpath = struct
  include Fpath
  include Fpath0
  include Path.Export
end

module Absolute_path = Path.Absolute_path
module Relative_path = Path.Relative_path
module Fsegment = Fsegment
