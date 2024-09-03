let%expect_test "of_string" =
  let test str =
    print_s [%sexp (File_name.of_string str : (File_name.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg ": invalid file name")) |}];
  test "a";
  [%expect {| (Ok a) |}];
  test ".a";
  [%expect {| (Ok .a) |}];
  test "..";
  [%expect {| (Ok ..) |}];
  test "/";
  [%expect {| (Error (Msg "/: invalid file name")) |}];
  test "a/b";
  [%expect {| (Error (Msg "a/b: invalid file name")) |}];
  test "a\000b";
  [%expect {| (Error (Msg "a\000b: invalid file name")) |}];
  ()
;;

let%expect_test "hard coded" =
  List.iter
    ~f:(fun (name, t) ->
      print_endline (Printf.sprintf "%10s: " name ^ File_name.to_string t))
    File_name.[ "dot", dot; "dot_dot", dot_dot; "dot_git", dot_git; "dot_hg", dot_hg ];
  [%expect {|
        dot: .
    dot_dot: ..
    dot_git: .git
     dot_hg: .hg |}];
  ()
;;
