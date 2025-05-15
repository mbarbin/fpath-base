open! Stdlib_compat

type t = Fpath.t

let sexp_of_t t = Sexplib0.Sexp.Atom (Fpath.to_string t)
let compare = Fpath.compare
let equal = Fpath.equal
let hash t = String.hash (Fpath.to_string t)
let seeded_hash seed t = String.seeded_hash seed (Fpath.to_string t)
