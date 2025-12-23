(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

let%expect_test "sexp_of_t" =
  print_endline
    (Sexplib0.Sexp.to_string_hum
       (Absolute_path.sexp_of_t (Absolute_path.v "/hello/path/to/sexp")));
  [%expect {| /hello/path/to/sexp |}];
  ()
;;

let%expect_test "of_string" =
  let test str =
    print_dyn
      (or_msg_to_dyn
         (fun p -> Dyn.string (Absolute_path.to_string p))
         (Absolute_path.of_string str))
  in
  test "";
  [%expect {| Error (Msg "\"\": invalid path") |}];
  test "/";
  [%expect {| Ok "/" |}];
  test "a";
  [%expect {| Error (Msg "\"a\" is not an absolute path") |}];
  test "/a/b/../..";
  [%expect {| Ok "/" |}];
  ()
;;

let%expect_test "v" =
  require_does_raise (fun () -> Absolute_path.v "");
  [%expect {| Invalid_argument("Absolute_path.v: \"\": invalid path") |}];
  ()
;;

let%expect_test "of_fpath" =
  let test_fpath f =
    let t = Absolute_path.of_fpath f in
    if Option.is_none t then print_endline "Not an absolute path.";
    Option.iter t ~f:(fun t ->
      print_endline (Absolute_path.to_string t);
      let f' = Absolute_path.to_fpath t in
      if Fpath.equal f f'
      then
        print_dyn
          (Dyn.Tuple
             [ String "Does roundtrip."; Record [ "f", String (f |> Fpath.to_string) ] ])
      else
        print_dyn
          (Dyn.Tuple
             [ String "Does not roundtrip."
             ; Record
                 [ "f", String (f |> Fpath.to_string)
                 ; "f'", String (f' |> Fpath.to_string)
                 ]
             ]))
  in
  test_fpath (Fpath.v "/foo/bar");
  [%expect
    {|
    /foo/bar
    ("Does roundtrip.", { f = "/foo/bar" })
    |}];
  test_fpath (Fpath.v "/foo/bar/");
  [%expect
    {|
    /foo/bar/
    ("Does roundtrip.", { f = "/foo/bar/" })
    |}];
  test_fpath (Fpath.v "/");
  [%expect
    {|
    /
    ("Does roundtrip.", { f = "/" })
    |}];
  test_fpath (Fpath.v "/.");
  [%expect
    {|
    /
    ("Does not roundtrip.", { f = "/."; f' = "/" })
    |}];
  test_fpath (Fpath.v "a/relative/path");
  [%expect {| Not an absolute path. |}];
  require_does_raise (fun () -> Fpath.v "");
  [%expect {| Invalid_argument("\"\": invalid path") |}];
  ()
;;

let%expect_test "append" =
  let abs = Absolute_path.v in
  let rel = Relative_path.v in
  let test a b = print_endline (Absolute_path.append a b |> Absolute_path.to_string) in
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
  (* Escaping relative paths cannot be created. *)
  require_does_raise (fun () ->
    (test (abs "/a/c") (rel "./../b/d/../c/.") [@coverage off]));
  [%expect
    {| Invalid_argument("Relative_path.v: path \"./../b/d/../c/.\" escapes above starting point") |}];
  require_does_raise (fun () ->
    (test (abs "/a/c") (rel "./../../../b/d/../c/.") [@coverage off]));
  [%expect
    {| Invalid_argument("Relative_path.v: path \"./../../../b/d/../c/.\" escapes above starting point") |}];
  ()
;;

let%expect_test "append - cannot escape base path" =
  (* [Absolute_path.append] has a guarantee: since [Relative_path.t] is
     guaranteed to never escape upward (cannot contain leading ".." segments),
     appending a relative path to an absolute path is guaranteed to produce a
     result that stays at or below the base path in the directory tree.

     This test demonstrates that append cannot escape the base path. *)
  let abs = Absolute_path.v in
  let rel = Relative_path.v in
  let test a b = print_endline (Absolute_path.append a b |> Absolute_path.to_string) in
  (* Appending a simple path stays below base. *)
  test (abs "/foo/bar") (rel "baz");
  [%expect {| /foo/bar/baz |}];
  (* Appending empty path returns base (effectively). *)
  test (abs "/foo/bar") (rel ".");
  [%expect {| /foo/bar/ |}];
  (* Paths with ".." in the middle (that don't escape) normalize correctly. *)
  test (abs "/foo/bar") (rel "a/../b");
  [%expect {| /foo/bar/b |}];
  test (abs "/foo/bar") (rel "a/b/../c");
  [%expect {| /foo/bar/a/c |}];
  (* Even complex normalization stays within or at the base path. *)
  test (abs "/foo/bar") (rel "a/b/c/../../d");
  [%expect {| /foo/bar/a/d |}];
  (* The result is always at or below /foo/bar - never above it. *)
  ()
;;

let%expect_test "append - v0.4.0 improvement" =
  (* Prior to v0.4.0, it was theoretically possible to construct
     [Relative_path.t] values that escaped upward. Now, [Relative_path.v]
     rejects such paths at construction time, making append inherently safer.

     This test verifies that attempts to create escaping relative paths fail
     before they can be used with append. *)
  (* These attempts to create escaping relative paths now fail. *)
  require_does_raise (fun () -> Relative_path.v "..");
  [%expect
    {| Invalid_argument("Relative_path.v: path \"..\" escapes above starting point") |}];
  require_does_raise (fun () -> Relative_path.v "../foo");
  [%expect
    {| Invalid_argument("Relative_path.v: path \"../foo\" escapes above starting point") |}];
  require_does_raise (fun () -> Relative_path.v "a/../..");
  [%expect
    {| Invalid_argument("Relative_path.v: path \"a/../..\" escapes above starting point") |}];
  (* Therefore, append can never receive an escaping path.

     Before v0.4.0, if you could construct [Relative_path.v "../foo"], then:
     [Absolute_path.append (abs "/a/b") (hypothetical_escaping_path)] might have
     produced [/a/foo] (escaping above [/a/b]).

     Now this is prevented at construction time. *)
  ()
;;

let%expect_test "extend" =
  let abs = Absolute_path.v in
  let file str = str |> Fsegment.v in
  let test a b = print_endline (Absolute_path.extend a b |> Absolute_path.to_string) in
  require_does_raise (fun () : Fsegment.t -> file "a/b");
  [%expect {| Invalid_argument("Fsegment.v: invalid file segment \"a/b\"") |}];
  ignore (file ".." : Fsegment.t);
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
  test (abs "/") (file "..");
  [%expect {| / |}];
  ()
;;

let%expect_test "parent" =
  let abs = Absolute_path.v in
  let test path =
    let result = Absolute_path.parent path in
    print_dyn (Dyn.option (fun s -> Dyn.string (s |> Absolute_path.to_string)) result)
  in
  test (abs "/foo/bar");
  [%expect {| Some "/foo/" |}];
  test (abs "/foo/bar/");
  [%expect {| Some "/foo/" |}];
  test (abs "/foo/bar/../baz/../foo/");
  [%expect {| Some "/foo/" |}];
  test (abs "/foo");
  [%expect {| Some "/" |}];
  test (abs "/");
  [%expect {| None |}];
  test (abs "/.");
  [%expect {| None |}];
  ()
;;

let%expect_test "chop_prefix" =
  let abs = Absolute_path.v in
  let test prefix path =
    let result = Absolute_path.chop_prefix path ~prefix in
    print_dyn (Dyn.option (fun s -> Dyn.string (s |> Relative_path.to_string)) result)
  in
  test (abs "/foo") (abs "/foo/bar");
  [%expect {| Some "bar" |}];
  test (abs "/foo/") (abs "/foo/bar");
  [%expect {| Some "bar" |}];
  test (abs "/foo") (abs "/foo/bar/");
  [%expect {| Some "bar/" |}];
  test (abs "/foo/") (abs "/foo/bar/");
  [%expect {| Some "bar/" |}];
  test (abs "/foo/") (abs "/foo/");
  [%expect {| Some "./" |}];
  test (abs "/foo") (abs "/foo/");
  [%expect {| Some "./" |}];
  test (abs "/foo") (abs "/foo");
  [%expect {| Some "./" |}];
  test (abs "/foo/") (abs "/foo");
  [%expect {| None |}];
  test (abs "/foo/.") (abs "/foo/");
  [%expect {| Some "./" |}];
  test (abs "/foo/.") (abs "/foo");
  [%expect {| None |}];
  test (abs "/foo/") (abs "/foo/.");
  [%expect {| Some "./" |}];
  test (abs "/foo") (abs "/foo/.");
  [%expect {| Some "./" |}];
  test (abs "/foo/bar/") (abs "/foo/bar/");
  [%expect {| Some "./" |}];
  test (abs "/foo/bar") (abs "/foo/bar/");
  [%expect {| Some "./" |}];
  test (abs "/foo/bar") (abs "/foo/bar");
  [%expect {| Some "./" |}];
  test (abs "/foo/bar/") (abs "/foo/bar");
  [%expect {| None |}];
  test (abs "/foo") (abs "/foo/bar/baz");
  [%expect {| Some "bar/baz" |}];
  test (abs "/foo") (abs "/bar/baz");
  [%expect {| None |}];
  test (abs "/foo/bar") (abs "/foo");
  [%expect {| None |}];
  test (abs "/foo/bar") (abs "/foo/bar/baz");
  [%expect {| Some "baz" |}];
  test (abs "/foo/bar") (abs "/foo/bar/baz/qux");
  [%expect {| Some "baz/qux" |}];
  (* Paths are normalized before the function call. *)
  test (abs "/foo/bar") (abs "/foo/bar/../baz");
  [%expect {| None |}];
  test (abs "/foo/bar") (abs "/foo/sna/../bar/baz");
  [%expect {| Some "baz" |}];
  (* Beware of string prefix vs path prefix. *)
  test (abs "/foo/bar") (abs "/foo/bar-baz");
  [%expect {| None |}];
  test (abs "/") (abs "/foo/bar/baz");
  [%expect {| Some "foo/bar/baz" |}];
  test (abs "/") (abs "/foo/bar/baz/");
  [%expect {| Some "foo/bar/baz/" |}];
  ()
;;

let%expect_test "chop_suffix" =
  let abs = Absolute_path.v in
  let rel = Relative_path.v in
  let test path suffix =
    let result = Absolute_path.chop_suffix path ~suffix in
    print_dyn (Dyn.option (fun s -> Dyn.string (s |> Absolute_path.to_string)) result)
  in
  test (abs "/foo/bar") (rel "bar");
  [%expect {| Some "/foo/" |}];
  test (abs "/foo/bar") (rel "bar/");
  [%expect {| None |}];
  test (abs "/foo/bar/") (rel "bar");
  [%expect {| None |}];
  test (abs "/foo/bar/") (rel "bar/");
  [%expect {| Some "/foo/" |}];
  test (abs "/foo/bar") (rel ".");
  [%expect {| Some "/foo/bar" |}];
  test (abs "/foo/bar/") (rel ".");
  [%expect {| Some "/foo/bar/" |}];
  test (abs "/foo/bar/.") (rel ".");
  [%expect {| Some "/foo/bar/" |}];
  test (abs "/bar") (rel "foo/bar");
  [%expect {| None |}];
  test (abs "/foo/bar") (rel "foo/bar");
  [%expect {| Some "/" |}];
  test (abs "/foo/bar") (rel "bar/");
  [%expect {| None |}];
  test (abs "/foo/bar/") (rel "bar");
  [%expect {| None |}];
  test (abs "/foo/bar/") (rel "bar/");
  [%expect {| Some "/foo/" |}];
  test (abs "/foo/bar") (rel "baz");
  [%expect {| None |}];
  test (abs "/foo/bar/baz") (rel "bar/baz");
  [%expect {| Some "/foo/" |}];
  test (abs "/foo/bar/baz") (rel "baz/qux");
  [%expect {| None |}];
  test (abs "/foo/bar/baz") (rel ".");
  [%expect {| Some "/foo/bar/baz" |}];
  test (abs "/foo/bar/baz/") (rel ".");
  [%expect {| Some "/foo/bar/baz/" |}];
  test (abs "/foo/bar/baz") (rel "./");
  [%expect {| Some "/foo/bar/baz" |}];
  test (abs "/foo/bar/baz/") (rel "./");
  [%expect {| Some "/foo/bar/baz/" |}];
  test (abs "/foo/bar/baz") (rel "foo/../baz");
  [%expect {| Some "/foo/bar/" |}];
  (* Beware of string suffix vs path suffix. *)
  test (abs "/foo/bar-baz") (rel "-baz");
  [%expect {| None |}];
  ()
;;

let%expect_test "is_dir_path" =
  let abs = Absolute_path.v in
  let test path = print_dyn (Absolute_path.is_dir_path (abs path) |> Dyn.bool) in
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
    print_endline (Absolute_path.to_dir_path (abs path) |> Absolute_path.to_string)
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
    print_dyn
      (Dyn.Tuple
         [ Record
             [ "path", path |> Absolute_path.to_string |> Dyn.string
             ; "is_dir_path", is_dir_path |> Dyn.bool
             ]
         ; Record
             [ "path2", path2 |> Absolute_path.to_string |> Dyn.string
             ; "is_dir_path2", is_dir_path2 |> Dyn.bool
             ]
         ])
  in
  test (Absolute_path.v "/tmp/my-dir/");
  [%expect
    {|
    ({ path = "/tmp/my-dir/"; is_dir_path = true },
     { path2 = "/tmp/my-dir"; is_dir_path2 = false })
    |}];
  test (Absolute_path.v "/tmp/my-file");
  [%expect
    {|
    ({ path = "/tmp/my-file"; is_dir_path = false },
     { path2 = "/tmp/my-file"; is_dir_path2 = false })
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
  test ~root:(abs "/") (v "../../foo/bar");
  [%expect {| /foo/bar |}];
  ()
;;

let hashtbl_to_dyn key value table =
  let data = Hashtbl.to_alist table in
  Dyn.Map (List.map data ~f:(fun (k, v) -> key k, value v))
;;

let%expect_test "hashtbl" =
  let t = Hashtbl.create (module Absolute_path) in
  Hashtbl.set t ~key:(Absolute_path.v "/tmp/my-file") ~data:42;
  print_dyn (hashtbl_to_dyn (fun p -> Dyn.string (Absolute_path.to_string p)) Dyn.int t);
  [%expect {| map { "/tmp/my-file" : 42 } |}]
;;

module Pair = struct
  [@@@coverage off]

  type t =
    { a : Absolute_path.t
    ; b : Absolute_path.t
    }
  [@@deriving compare, hash, sexp_of]

  let to_dyn { a; b } =
    Dyn.Record
      [ "a", a |> Absolute_path.to_string |> Dyn.string
      ; "b", b |> Absolute_path.to_string |> Dyn.string
      ]
  ;;
end

let%expect_test "hash-fold-t" =
  let t = Hashtbl.create (module Pair) in
  Hashtbl.set
    t
    ~key:{ a = Absolute_path.v "/tmp/a"; b = Absolute_path.v "/tmp/a" }
    ~data:42;
  print_dyn (hashtbl_to_dyn Pair.to_dyn Dyn.int t);
  [%expect {| map { { a = "/tmp/a"; b = "/tmp/a" } : 42 } |}]
;;
