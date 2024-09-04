module Fpath : sig
  type t = Fpath_sexp0.Fpath.t [@@deriving hash]

  include module type of Fpath_sexp0.Fpath with type t := t
  include Comparable.S with type t := t
end

module Absolute_path : sig
  type t = Fpath_sexp0.Absolute_path.t [@@deriving hash]

  include module type of Fpath_sexp0.Absolute_path with type t := t
  include Comparable.S with type t := t
end

module Relative_path : sig
  type t = Fpath_sexp0.Relative_path.t [@@deriving hash]

  include module type of Fpath_sexp0.Relative_path with type t := t
  include Comparable.S with type t := t
end

module Fpart : sig
  type t = Fpath_sexp0.Fpart.t [@@deriving hash]

  include module type of Fpath_sexp0.Fpart with type t := t
  include Comparable.S with type t := t
end
