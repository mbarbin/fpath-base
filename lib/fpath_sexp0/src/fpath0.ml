type t = Fpath.t

let sexp_of_t t = Sexplib0.Sexp.Atom (Fpath.to_string t)
let compare = Fpath.compare
let equal = Fpath.equal
