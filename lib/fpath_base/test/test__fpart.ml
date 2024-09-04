let%expect_test "of_string" =
  let test str =
    print_s [%sexp (Fpart.of_string str : (Fpart.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg ": invalid file part")) |}];
  test "a";
  [%expect {| (Ok a) |}];
  test ".a";
  [%expect {| (Ok .a) |}];
  test "..";
  [%expect {| (Ok ..) |}];
  test "/";
  [%expect {| (Error (Msg "/: invalid file part")) |}];
  test "a/b";
  [%expect {| (Error (Msg "a/b: invalid file part")) |}];
  test "a\000b";
  [%expect {| (Error (Msg "a\000b: invalid file part")) |}];
  ()
;;

let%expect_test "hard coded" =
  List.iter
    ~f:(fun (name, t) -> print_endline (Printf.sprintf "%10s: " name ^ Fpart.to_string t))
    Fpart.[ "dot", dot; "dot_dot", dot_dot; "dot_git", dot_git; "dot_hg", dot_hg ];
  [%expect {|
        dot: .
    dot_dot: ..
    dot_git: .git
     dot_hg: .hg |}];
  ()
;;

let%expect_test "hashtbl" =
  let t = Hashtbl.create (module Fpart) in
  Hashtbl.set t ~key:(Fpart.v "my-file") ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Fpart).t)];
  [%expect {| ((my-file 42)) |}]
;;

module Pair = struct
  [@@@coverage off]

  type t =
    { a : Fpart.t
    ; b : Fpart.t
    }
  [@@deriving compare, hash, sexp_of]
end

let%expect_test "hash-fold-t" =
  let t = Hashtbl.create (module Pair) in
  Hashtbl.set t ~key:{ a = Fpart.v "a"; b = Fpart.v "b" } ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Pair).t)];
  [%expect {|
    ((
      ((a a)
       (b b))
      42))
    |}]
;;
