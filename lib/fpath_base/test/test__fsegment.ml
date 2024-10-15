let%expect_test "of_string" =
  let test str =
    print_s [%sexp (Fsegment.of_string str : (Fsegment.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg ": invalid file segment")) |}];
  test "a";
  [%expect {| (Ok a) |}];
  test ".a";
  [%expect {| (Ok .a) |}];
  test "..";
  [%expect {| (Ok ..) |}];
  test "/";
  [%expect {| (Error (Msg "/: invalid file segment")) |}];
  test "a/b";
  [%expect {| (Error (Msg "a/b: invalid file segment")) |}];
  test "a\000b";
  [%expect {| (Error (Msg "a\000b: invalid file segment")) |}];
  ()
;;

let%expect_test "hard coded" =
  List.iter
    ~f:(fun (name, t) ->
      print_endline (Printf.sprintf "%10s: " name ^ Fsegment.to_string t))
    Fsegment.[ "dot", dot; "dot_dot", dot_dot; "dot_git", dot_git; "dot_hg", dot_hg ];
  [%expect {|
        dot: .
    dot_dot: ..
    dot_git: .git
     dot_hg: .hg |}];
  ()
;;

let%expect_test "hashtbl" =
  let t = Hashtbl.create (module Fsegment) in
  Hashtbl.set t ~key:(Fsegment.v "my-file") ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Fsegment).t)];
  [%expect {| ((my-file 42)) |}]
;;

module Pair = struct
  [@@@coverage off]

  type t =
    { a : Fsegment.t
    ; b : Fsegment.t
    }
  [@@deriving compare, hash, sexp_of]
end

let%expect_test "hash-fold-t" =
  let t = Hashtbl.create (module Pair) in
  Hashtbl.set t ~key:{ a = Fsegment.v "a"; b = Fsegment.v "b" } ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Pair).t)];
  [%expect {|
    ((
      ((a a)
       (b b))
      42))
    |}]
;;
