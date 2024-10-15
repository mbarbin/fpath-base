let%expect_test "of_string" =
  let test str =
    print_s
      [%sexp
        (Relative_path.of_string str : (Relative_path.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg "\"\": invalid path")) |}];
  test ".";
  [%expect {| (Ok ./) |}];
  test "/a";
  [%expect {| (Error (Msg "\"/a\": not a relative path")) |}];
  test "a/b/../..";
  [%expect {| (Ok ./) |}];
  ()
;;

let%expect_test "v" =
  require_does_raise [%here] (fun () -> Relative_path.v "");
  [%expect {| (Invalid_argument "\"\": invalid path") |}];
  ()
;;

let%expect_test "of_fpath" =
  let test_fpath f =
    let t = Relative_path.of_fpath f in
    if Option.is_none t then print_s [%sexp "not a relative path"];
    Option.iter t ~f:(fun t ->
      print_endline (Relative_path.to_string t);
      let f' = Relative_path.to_fpath t in
      if Fpath.equal f f'
      then print_s [%sexp "does roundtrip", { f : Fpath.t }]
      else print_s [%sexp "does not roundtrip", { f : Fpath.t; f' : Fpath.t }])
  in
  test_fpath (Fpath.v "foo/bar");
  [%expect {|
    foo/bar
    ("does roundtrip" ((f foo/bar))) |}];
  test_fpath (Fpath.v "foo/bar/");
  [%expect {|
      foo/bar/
      ("does roundtrip" ((f foo/bar/))) |}];
  test_fpath (Fpath.v ".");
  [%expect {|
    ./
    ("does not roundtrip" (
      (f  .)
      (f' ./))) |}];
  test_fpath (Fpath.v "./");
  [%expect {|
      ./
      ("does roundtrip" ((f ./))) |}];
  test_fpath (Fpath.v "/an/absolute/path");
  [%expect {| "not a relative path" |}];
  ()
;;

let%expect_test "append" =
  let rel = Relative_path.v in
  let test a b = print_s [%sexp (Relative_path.append a b : Relative_path.t)] in
  test Relative_path.empty Relative_path.empty;
  [%expect {| ./ |}];
  test Relative_path.empty Relative_path.empty;
  [%expect {| ./ |}];
  test (rel "./a") Relative_path.empty;
  [%expect {| a/ |}];
  test (rel "./a/") Relative_path.empty;
  [%expect {| a/ |}];
  test (rel "a") Relative_path.empty;
  [%expect {| a/ |}];
  test (rel "a/") Relative_path.empty;
  [%expect {| a/ |}];
  test Relative_path.empty (rel "./a");
  [%expect {| a |}];
  test Relative_path.empty (rel "./a/");
  [%expect {| a/ |}];
  test Relative_path.empty (rel "a");
  [%expect {| a |}];
  test Relative_path.empty (rel "a/");
  [%expect {| a/ |}];
  test (rel "a/b") (rel "c/d");
  [%expect {| a/b/c/d |}];
  test (rel "a/b") (rel "c/d/");
  [%expect {| a/b/c/d/ |}];
  test (rel "a/b/") (rel "c/d");
  [%expect {| a/b/c/d |}];
  test Relative_path.empty (rel "a/b/c");
  [%expect {| a/b/c |}];
  test (rel "./a/b/../c/.") (rel "d/e");
  [%expect {| a/c/d/e |}];
  test (rel "./../b/d/../c/.") (rel "e/f");
  [%expect {| ../b/c/e/f |}];
  test (rel "./../../../b/d/../c/.") (rel "f/g");
  [%expect {| ../../../b/c/f/g |}];
  test (rel "x/y/z") (rel "../../a/b/c");
  [%expect {| x/a/b/c |}];
  test (rel "a/b/c") (rel "../../../x/y/z");
  [%expect {| x/y/z |}];
  test (rel "a/b/c") (rel "../../../../x/y/z");
  [%expect {| ../x/y/z |}];
  test (rel "a/b/c") (rel "../../../../../../x/y/z");
  [%expect {| ../../../x/y/z |}];
  ()
;;

let%expect_test "extend" =
  let rel = Relative_path.v in
  let file str = str |> Fsegment.v in
  let test a b = print_s [%sexp (Relative_path.extend a b : Relative_path.t)] in
  require_does_raise [%here] (fun () : Fsegment.t -> file "a/b");
  [%expect {| (Invalid_argument "a/b: invalid file segment") |}];
  require_does_not_raise [%here] (fun () -> ignore (file ".." : Fsegment.t));
  [%expect {| |}];
  test Relative_path.empty (file "a");
  [%expect {| a |}];
  test Relative_path.empty (file ".a");
  [%expect {| .a |}];
  test (rel "a") (file "b");
  [%expect {| a/b |}];
  test (rel "a/b") (file ".");
  [%expect {| a/b/ |}];
  test (rel "a/b/") (file ".");
  [%expect {| a/b/ |}];
  test (rel "a") (file "b");
  [%expect {| a/b |}];
  test (rel "a/b") (file ".");
  [%expect {| a/b/ |}];
  test (rel "a/b") (file "c");
  [%expect {| a/b/c |}];
  test (rel "a/b/") (file "c");
  [%expect {| a/b/c |}];
  test (rel "a/b") (file "..");
  [%expect {| a/ |}];
  test (rel "a/bar/foo") (file "..");
  [%expect {| a/bar/ |}];
  ()
;;

let%expect_test "parent" =
  let rel = Relative_path.v in
  let test path =
    let result = Relative_path.parent path in
    print_s [%sexp (result : Relative_path.t option)]
  in
  test (rel "foo/bar");
  [%expect {| (foo/) |}];
  test (rel "foo/bar/");
  [%expect {| (foo/) |}];
  test (rel "foo");
  [%expect {| (./) |}];
  test Relative_path.empty;
  [%expect {| (./../) |}];
  ()
;;

let%expect_test "of_list" =
  let test files =
    let result = Relative_path.of_list (List.map files ~f:Fsegment.v) in
    print_s [%sexp (result : Relative_path.t)]
  in
  test [];
  [%expect {| ./ |}];
  test [ "a" ];
  [%expect {| a |}];
  test [ "." ];
  [%expect {| ./ |}];
  test [ ".." ];
  [%expect {| ../ |}];
  test [ "a"; ".." ];
  [%expect {| ./ |}];
  test [ "a"; "." ];
  [%expect {| a/ |}];
  test [ "a"; "b"; ".."; "c" ];
  [%expect {| a/c |}];
  test [ "a"; "b"; "c"; "d" ];
  [%expect {| a/b/c/d |}];
  ()
;;

let%expect_test "chop_prefix" =
  let rel = Relative_path.v in
  let test prefix path =
    let result = Relative_path.chop_prefix path ~prefix in
    print_s [%sexp (result : Relative_path.t option)]
  in
  test (rel "foo") (rel "foo/bar");
  [%expect {| (bar) |}];
  test (rel "foo") (rel "foo/bar/");
  [%expect {| (bar/) |}];
  test (rel "foo/") (rel "foo/bar");
  [%expect {| (bar) |}];
  test (rel "foo/") (rel "foo/bar/");
  [%expect {| (bar/) |}];
  test (rel "foo/") (rel "foo");
  [%expect {| () |}];
  test (rel "foo") (rel "foo/bar/baz");
  [%expect {| (bar/baz) |}];
  test (rel "foo") (rel "bar/baz");
  [%expect {| () |}];
  test (rel "foo/bar") (rel "foo");
  [%expect {| () |}];
  test (rel "foo/bar") (rel "foo");
  [%expect {| () |}];
  test (rel "foo/bar") (rel "foo/bar/baz");
  [%expect {| (baz) |}];
  test (rel "foo/bar") (rel "foo/bar/baz/qux");
  [%expect {| (baz/qux) |}];
  (* Paths are normalized before the function call. *)
  test (rel "foo/bar") (rel "foo/bar/../baz");
  [%expect {| () |}];
  test (rel "foo/bar") (rel "foo/sna/../bar/baz");
  [%expect {| (baz) |}];
  (* Beware of string prefix vs path prefix *)
  test (rel "foo/bar") (rel "foo/bar-baz");
  [%expect {| () |}];
  ()
;;

let%expect_test "chop_suffix" =
  let rel = Relative_path.v in
  let test path suffix =
    let result = Relative_path.chop_suffix path ~suffix in
    print_s [%sexp (result : Relative_path.t option)]
  in
  test (rel "foo/bar") (rel "bar");
  [%expect {| (foo) |}];
  test (rel "foo/bar") (rel "bar/");
  [%expect {| () |}];
  test (rel "foo/bar/") (rel "bar");
  [%expect {| () |}];
  test (rel "foo/bar/") (rel "bar/");
  [%expect {| (foo) |}];
  test (rel "foo/bar") Relative_path.empty;
  [%expect {| () |}];
  test (rel "foo/bar/") Relative_path.empty;
  [%expect {| () |}];
  test (rel "foo/bar/.") Relative_path.empty;
  [%expect {| () |}];
  test (rel "bar") (rel "foo/bar");
  [%expect {| () |}];
  test (rel "foo/bar") (rel "foo/bar");
  [%expect {| (./) |}];
  test (rel "foo/bar") (rel "baz");
  [%expect {| () |}];
  test (rel "foo/bar/baz") (rel "bar/baz");
  [%expect {| (foo) |}];
  test (rel "foo/bar/baz") (rel "baz/qux");
  [%expect {| () |}];
  test (rel "foo/bar/baz") Relative_path.empty;
  [%expect {| () |}];
  test (rel "foo/bar/baz") (rel "..");
  [%expect {| () |}];
  test (rel "foo/bar/baz") (rel "foo/../baz");
  [%expect {| (foo/bar) |}];
  (* Beware of string suffix vs path suffix *)
  test (rel "foo/bar-baz") (rel "-baz");
  [%expect {| () |}];
  ()
;;

let%expect_test "is_dir_path" =
  let rel = Relative_path.v in
  let test path = print_s [%sexp (Relative_path.is_dir_path (rel path) : bool)] in
  test "foo/bar";
  [%expect {| false |}];
  test "foo/bar/";
  [%expect {| true |}];
  test "foo/bar/../baz/../foo/";
  [%expect {| true |}];
  test "foo";
  [%expect {| false |}];
  test "./";
  [%expect {| true |}];
  ()
;;

let%expect_test "to_dir_path" =
  let rel = Relative_path.v in
  let test path =
    print_s [%sexp (Relative_path.to_dir_path (rel path) : Relative_path.t)]
  in
  test "foo/bar";
  [%expect {| foo/bar/ |}];
  test "foo/bar/";
  [%expect {| foo/bar/ |}];
  test "foo/bar/../baz/../foo/";
  [%expect {| foo/foo/ |}];
  test "foo";
  [%expect {| foo/ |}];
  test "./";
  [%expect {| ./ |}];
  ()
;;

let%expect_test "rem_empty_seg" =
  let test path =
    let is_dir_path = Relative_path.is_dir_path path in
    let path2 = Relative_path.rem_empty_seg path in
    let is_dir_path2 = Relative_path.is_dir_path path2 in
    print_s
      [%sexp
        { path : Relative_path.t; is_dir_path : bool }
        , { path2 : Relative_path.t; is_dir_path2 : bool }]
  in
  test (Relative_path.v "tmp/my-dir/");
  [%expect
    {|
    (((path  tmp/my-dir/) (is_dir_path  true))
     ((path2 tmp/my-dir)  (is_dir_path2 false)))
    |}];
  test (Relative_path.v "tmp/my-file");
  [%expect
    {|
    (((path  tmp/my-file) (is_dir_path  false))
     ((path2 tmp/my-file) (is_dir_path2 false)))
    |}];
  ()
;;

let%expect_test "hashtbl" =
  let t = Hashtbl.create (module Relative_path) in
  Hashtbl.set t ~key:(Relative_path.v "path/to/my-file") ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Relative_path).t)];
  [%expect {| ((path/to/my-file 42)) |}]
;;

module Pair = struct
  [@@@coverage off]

  type t =
    { a : Relative_path.t
    ; b : Relative_path.t
    }
  [@@deriving compare, hash, sexp_of]
end

let%expect_test "hash-fold-t" =
  let t = Hashtbl.create (module Pair) in
  Hashtbl.set
    t
    ~key:{ a = Relative_path.v "path/to/a"; b = Relative_path.v "path/to/b" }
    ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Pair).t)];
  [%expect {|
    ((
      ((a path/to/a)
       (b path/to/b))
      42))
    |}]
;;
