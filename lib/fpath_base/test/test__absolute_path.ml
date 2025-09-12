(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

let%expect_test "of_string" =
  let test str =
    print_s
      [%sexp
        (Absolute_path.of_string str : (Absolute_path.t, [ `Msg of string ]) Result.t)]
  in
  test "";
  [%expect {| (Error (Msg "\"\": invalid path")) |}];
  test "/";
  [%expect {| (Ok /) |}];
  test "a";
  [%expect {| (Error (Msg "\"a\": not an absolute path")) |}];
  test "/a/b/../..";
  [%expect {| (Ok /) |}];
  ()
;;

let%expect_test "v" =
  require_does_raise [%here] (fun () -> Absolute_path.v "");
  [%expect {| (Invalid_argument "\"\": invalid path") |}];
  ()
;;

let%expect_test "of_fpath" =
  let test_fpath f =
    let t = Absolute_path.of_fpath f in
    if Option.is_none t then print_s [%sexp "not an absolute path"];
    Option.iter t ~f:(fun t ->
      print_endline (Absolute_path.to_string t);
      let f' = Absolute_path.to_fpath t in
      if Fpath.equal f f'
      then print_s [%sexp "does roundtrip", { f : Fpath.t }]
      else print_s [%sexp "does not roundtrip", { f : Fpath.t; f' : Fpath.t }])
  in
  test_fpath (Fpath.v "/foo/bar");
  [%expect
    {|
    /foo/bar
    ("does roundtrip" ((f /foo/bar))) |}];
  test_fpath (Fpath.v "/foo/bar/");
  [%expect
    {|
      /foo/bar/
      ("does roundtrip" ((f /foo/bar/))) |}];
  test_fpath (Fpath.v "/");
  [%expect
    {|
    /
    ("does roundtrip" ((f /))) |}];
  test_fpath (Fpath.v "/.");
  [%expect
    {|
      /
      ("does not roundtrip" (
        (f  /.)
        (f' /))) |}];
  test_fpath (Fpath.v "a/relative/path");
  [%expect {| "not an absolute path" |}];
  require_does_raise [%here] (fun () -> Fpath.v "");
  [%expect {| (Invalid_argument "\"\": invalid path") |}];
  ()
;;

let%expect_test "append" =
  let abs = Absolute_path.v in
  let rel = Relative_path.v in
  let test a b = print_s [%sexp (Absolute_path.append a b : Absolute_path.t)] in
  test (abs "/a") (rel "b");
  [%expect {| /a/b |}];
  test (abs "/a") (rel "b/");
  [%expect {| /a/b/ |}];
  test (abs "/a") (rel ".");
  [%expect {| /a/ |}];
  test (abs "/a/") (rel ".");
  [%expect {| /a/ |}];
  test (abs "/a") (rel "./");
  [%expect {| /a/ |}];
  test (abs "/") (rel "a/b");
  [%expect {| /a/b |}];
  test Absolute_path.root (rel ".");
  [%expect {| / |}];
  test Absolute_path.root (rel "./a/b/c");
  [%expect {| /a/b/c |}];
  test (abs "/") (rel "./a/b/../c/.");
  [%expect {| /a/c/ |}];
  test (abs "/a/c") (rel "./../b/d/../c/.");
  [%expect {| /a/b/c/ |}];
  test (abs "/a/c") (rel "./../../../b/d/../c/.");
  [%expect {| /b/c/ |}];
  ()
;;

let%expect_test "extend" =
  let abs = Absolute_path.v in
  let file str = str |> Fsegment.v in
  let test a b = print_s [%sexp (Absolute_path.extend a b : Absolute_path.t)] in
  require_does_raise [%here] (fun () : Fsegment.t -> file "a/b");
  [%expect {| (Invalid_argument "a/b: invalid file segment") |}];
  require_does_not_raise [%here] (fun () -> ignore (file ".." : Fsegment.t));
  [%expect {||}];
  test (abs "/") (file "a");
  [%expect {| /a |}];
  test Absolute_path.root (file "..");
  [%expect {| / |}];
  test Absolute_path.root (file ".");
  [%expect {| / |}];
  test Absolute_path.root (file ".a");
  [%expect {| /.a |}];
  test (abs "/a") (file "b");
  [%expect {| /a/b |}];
  test (abs "/a/b") (file ".");
  [%expect {| /a/b/ |}];
  test (abs "/a/b") (file "c");
  [%expect {| /a/b/c |}];
  test (abs "/a/b") (file "..");
  [%expect {| /a/ |}];
  test (abs "/a/bar/foo") (file "..");
  [%expect {| /a/bar/ |}];
  ()
;;

let%expect_test "parent" =
  let abs = Absolute_path.v in
  let test path =
    let result = Absolute_path.parent path in
    print_s [%sexp (result : Absolute_path.t option)]
  in
  test (abs "/foo/bar");
  [%expect {| (/foo/) |}];
  test (abs "/foo/bar/");
  [%expect {| (/foo/) |}];
  test (abs "/foo/bar/../baz/../foo/");
  [%expect {| (/foo/) |}];
  test (abs "/foo");
  [%expect {| (/) |}];
  test (abs "/");
  [%expect {| () |}];
  test (abs "/.");
  [%expect {| () |}];
  ()
;;

let%expect_test "chop_prefix" =
  let abs = Absolute_path.v in
  let test prefix path =
    let result = Absolute_path.chop_prefix path ~prefix in
    print_s [%sexp (result : Relative_path.t option)]
  in
  test (abs "/foo") (abs "/foo/bar");
  [%expect {| (bar) |}];
  test (abs "/foo/") (abs "/foo/bar");
  [%expect {| (bar) |}];
  test (abs "/foo") (abs "/foo/bar/");
  [%expect {| (bar/) |}];
  test (abs "/foo/") (abs "/foo/bar/");
  [%expect {| (bar/) |}];
  test (abs "/foo/") (abs "/foo/");
  [%expect {| (./) |}];
  test (abs "/foo") (abs "/foo/");
  [%expect {| (./) |}];
  test (abs "/foo") (abs "/foo");
  [%expect {| (./) |}];
  test (abs "/foo/") (abs "/foo");
  [%expect {| () |}];
  test (abs "/foo") (abs "/foo/bar/baz");
  [%expect {| (bar/baz) |}];
  test (abs "/foo") (abs "/bar/baz");
  [%expect {| () |}];
  test (abs "/foo/bar") (abs "/foo");
  [%expect {| () |}];
  test (abs "/foo/bar") (abs "/foo/bar/baz");
  [%expect {| (baz) |}];
  test (abs "/foo/bar") (abs "/foo/bar/baz/qux");
  [%expect {| (baz/qux) |}];
  (* Paths are normalized before the function call. *)
  test (abs "/foo/bar") (abs "/foo/bar/../baz");
  [%expect {| () |}];
  test (abs "/foo/bar") (abs "/foo/sna/../bar/baz");
  [%expect {| (baz) |}];
  (* Beware of string prefix vs path prefix *)
  test (abs "/foo/bar") (abs "/foo/bar-baz");
  [%expect {| () |}];
  ()
;;

let%expect_test "chop_suffix" =
  let abs = Absolute_path.v in
  let rel = Relative_path.v in
  let test path suffix =
    let result = Absolute_path.chop_suffix path ~suffix in
    print_s [%sexp (result : Absolute_path.t option)]
  in
  test (abs "/foo/bar") (rel "bar");
  [%expect {| (/foo) |}];
  test (abs "/foo/bar") (rel "bar/");
  [%expect {| () |}];
  test (abs "/foo/bar/") (rel "bar");
  [%expect {| () |}];
  test (abs "/foo/bar/") (rel "bar/");
  [%expect {| (/foo) |}];
  test (abs "/foo/bar") (rel ".");
  [%expect {| () |}];
  test (abs "/foo/bar/") (rel ".");
  [%expect {| () |}];
  test (abs "/foo/bar/.") (rel ".");
  [%expect {| () |}];
  test (abs "/bar") (rel "foo/bar");
  [%expect {| () |}];
  test (abs "/foo/bar") (rel "foo/bar");
  [%expect {| (/) |}];
  test (abs "/foo/bar") (rel "bar/");
  [%expect {| () |}];
  test (abs "/foo/bar/") (rel "bar");
  [%expect {| () |}];
  test (abs "/foo/bar/") (rel "bar/");
  [%expect {| (/foo) |}];
  test (abs "/foo/bar") (rel "baz");
  [%expect {| () |}];
  test (abs "/foo/bar/baz") (rel "bar/baz");
  [%expect {| (/foo) |}];
  test (abs "/foo/bar/baz") (rel "baz/qux");
  [%expect {| () |}];
  test (abs "/foo/bar/baz") (rel ".");
  [%expect {| () |}];
  test (abs "/foo/bar/baz/") (rel ".");
  [%expect {| () |}];
  test (abs "/foo/bar/baz") (rel "./");
  [%expect {| () |}];
  test (abs "/foo/bar/baz/") (rel "./");
  [%expect {| () |}];
  test (abs "/foo/bar/baz") (rel "..");
  [%expect {| () |}];
  test (abs "/foo/bar/baz") (rel "foo/../baz");
  [%expect {| (/foo/bar) |}];
  (* Beware of string suffix vs path suffix *)
  test (abs "/foo/bar-baz") (rel "-baz");
  [%expect {| () |}];
  ()
;;

let%expect_test "is_dir_path" =
  let abs = Absolute_path.v in
  let test path = print_s [%sexp (Absolute_path.is_dir_path (abs path) : bool)] in
  test "/foo/bar";
  [%expect {| false |}];
  test "/foo/bar/";
  [%expect {| true |}];
  test "/foo/bar/../baz/../foo/";
  [%expect {| true |}];
  test "/foo";
  [%expect {| false |}];
  test "/";
  [%expect {| true |}];
  test "/.";
  [%expect {| true |}];
  ()
;;

let%expect_test "to_dir_path" =
  let abs = Absolute_path.v in
  let test path =
    print_s [%sexp (Absolute_path.to_dir_path (abs path) : Absolute_path.t)]
  in
  test "/foo/bar";
  [%expect {| /foo/bar/ |}];
  test "/foo/bar/";
  [%expect {| /foo/bar/ |}];
  test "/foo/bar/../baz/../foo/";
  [%expect {| /foo/foo/ |}];
  test "/foo";
  [%expect {| /foo/ |}];
  test "/";
  [%expect {| / |}];
  test "/.";
  [%expect {| / |}];
  ()
;;

let%expect_test "rem_empty_seg" =
  let test path =
    let is_dir_path = Absolute_path.is_dir_path path in
    let path2 = Absolute_path.rem_empty_seg path in
    let is_dir_path2 = Absolute_path.is_dir_path path2 in
    print_s
      [%sexp
        { path : Absolute_path.t; is_dir_path : bool }
      , { path2 : Absolute_path.t; is_dir_path2 : bool }]
  in
  test (Absolute_path.v "/tmp/my-dir/");
  [%expect
    {|
    (((path  /tmp/my-dir/) (is_dir_path  true))
     ((path2 /tmp/my-dir)  (is_dir_path2 false)))
    |}];
  test (Absolute_path.v "/tmp/my-file");
  [%expect
    {|
    (((path  /tmp/my-file) (is_dir_path  false))
     ((path2 /tmp/my-file) (is_dir_path2 false)))
    |}];
  ()
;;

let%expect_test "relativize" =
  let v str = str |> Fpath.v in
  let abs = Absolute_path.v in
  let test ~root b =
    print_endline (Absolute_path.relativize ~root b |> Absolute_path.to_string)
  in
  test ~root:(abs "/a/b") (v "/");
  [%expect {| / |}];
  test ~root:(abs "/a/b") (v ".");
  [%expect {| /a/b/ |}];
  test ~root:(abs "/a/b") (v "../foo/bar");
  [%expect {| /a/foo/bar |}];
  test ~root:(abs "/") (v "../foo/bar");
  [%expect {| /foo/bar |}];
  ()
;;

let%expect_test "hashtbl" =
  let t = Hashtbl.create (module Absolute_path) in
  Hashtbl.set t ~key:(Absolute_path.v "/tmp/my-file") ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Absolute_path).t)];
  [%expect {| ((/tmp/my-file 42)) |}]
;;

module Pair = struct
  [@@@coverage off]

  type t =
    { a : Absolute_path.t
    ; b : Absolute_path.t
    }
  [@@deriving compare, hash, sexp_of]
end

let%expect_test "hash-fold-t" =
  let t = Hashtbl.create (module Pair) in
  Hashtbl.set
    t
    ~key:{ a = Absolute_path.v "/tmp/a"; b = Absolute_path.v "/tmp/a" }
    ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Pair).t)];
  [%expect
    {|
    ((
      ((a /tmp/a)
       (b /tmp/a))
      42))
    |}]
;;
