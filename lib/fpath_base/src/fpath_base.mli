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

module File_name : sig
  type t = Fpath_sexp0.File_name.t [@@deriving hash]

  include module type of Fpath_sexp0.File_name with type t := t
  include Comparable.S with type t := t
end
