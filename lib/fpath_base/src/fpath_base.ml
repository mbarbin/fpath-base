module Fpath = struct
  module T = Fpath_sexp0.Fpath
  include T
  include Comparable.Make (T)

  let hash t = String.hash (T.to_string t)
  let hash_fold_t state t = String.hash_fold_t state (T.to_string t)
end

module Absolute_path = struct
  module T = Fpath_sexp0.Absolute_path
  include T
  include Comparable.Make (T)

  let hash t = String.hash (T.to_string t)
  let hash_fold_t state t = String.hash_fold_t state (T.to_string t)
end

module Relative_path = struct
  module T = Fpath_sexp0.Relative_path
  include T
  include Comparable.Make (T)

  let hash t = String.hash (T.to_string t)
  let hash_fold_t state t = String.hash_fold_t state (T.to_string t)
end

module Fsegment = struct
  module T = Fpath_sexp0.Fsegment
  include T
  include Comparable.Make (T)

  let hash t = String.hash (T.to_string t)
  let hash_fold_t state t = String.hash_fold_t state (T.to_string t)
end

module Fpart = Fsegment
