(*_********************************************************************************)
(*_  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*_  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT                                                 *)
(*_********************************************************************************)

module Fpath : sig
  type t = Fpath.t

  include module type of Fpath_sexp0.Fpath with type t := t
  include Comparable.S with type t := t

  val hash : t -> int
  val hash_fold_t : Hash.state -> t -> Hash.state
end

module Absolute_path : sig
  type t = Fpath_sexp0.Absolute_path.t

  include module type of Fpath_sexp0.Absolute_path with type t := t
  include Comparable.S with type t := t

  val hash : t -> int
  val hash_fold_t : Hash.state -> t -> Hash.state
end

module Relative_path : sig
  type t = Fpath_sexp0.Relative_path.t

  include module type of Fpath_sexp0.Relative_path with type t := t
  include Comparable.S with type t := t

  val hash : t -> int
  val hash_fold_t : Hash.state -> t -> Hash.state
end

module Fsegment : sig
  type t = Fpath_sexp0.Fsegment.t

  include module type of Fpath_sexp0.Fsegment with type t := t
  include Comparable.S with type t := t

  val hash : t -> int
  val hash_fold_t : Hash.state -> t -> Hash.state
end
