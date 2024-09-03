type t = string

let compare = String.compare
let equal = String.equal
let sexp_of_t t = Sexplib0.Sexp.Atom t

let invariant t =
  String.length t > 0
  && String.for_all (fun c -> not (Char.equal c '/' || Char.equal c '\000')) t
;;

let to_string t = t

let of_string s =
  if invariant s then Ok s else Error (`Msg (Printf.sprintf "%s: invalid file name" s))
;;

let v t =
  match of_string t with
  | Ok t -> t
  | Error (`Msg s) -> invalid_arg s
;;

let dot = v "."
let dot_dot = v ".."
let dot_git = v ".git"
let dot_hg = v ".hg"
