(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

let%expect_test "sexp_of_t" =
  print_endline
    (Sexplib0.Sexp.to_string_hum
       (Relative_path.sexp_of_t (Relative_path.v "hello/path/to/sexp")));
  [%expect {| hello/path/to/sexp |}];
  ()
;;

let%expect_test "of_string" =
  let test str =
    print_dyn
      (or_msg_to_dyn
         (fun p -> Dyn.string (Relative_path.to_string p))
         (Relative_path.of_string str))
  in
  test "";
  [%expect {| Error (Msg "\"\": invalid path") |}];
  test ".";
  [%expect {| Ok "./" |}];
  test "/a";
  [%expect {| Error (Msg "\"/a\" is not a relative path") |}];
  test "/a/../..";
  [%expect {| Error (Msg "\"/a/../..\" is not a relative path") |}];
  test "a/b/../..";
  [%expect {| Ok "./" |}];
  (* Paths that escape upward are rejected. *)
  test "..";
  [%expect {| Error (Msg "path \"..\" escapes above starting point") |}];
  test "../a";
  [%expect {| Error (Msg "path \"../a\" escapes above starting point") |}];
  test "a/../..";
  [%expect {| Error (Msg "path \"a/../..\" escapes above starting point") |}];
  ()
;;

let%expect_test "v" =
  require_does_raise (fun () -> Relative_path.v "");
  [%expect {| Invalid_argument("Relative_path.v: \"\": invalid path") |}];
  require_does_raise (fun () -> Relative_path.v "..");
  [%expect
    {| Invalid_argument("Relative_path.v: path \"..\" escapes above starting point") |}];
  require_does_raise (fun () -> Relative_path.v "../a");
  [%expect
    {| Invalid_argument("Relative_path.v: path \"../a\" escapes above starting point") |}];
  ()
;;

let%expect_test "of_fpath" =
  let test_fpath f =
    let t = Relative_path.of_fpath f in
    if Option.is_none t then print_endline "Not a relative path.";
    Option.iter t ~f:(fun t ->
      print_endline (Relative_path.to_string t);
      let f' = Relative_path.to_fpath t in
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
  test_fpath (Fpath.v "foo/bar");
  [%expect
    {|
    foo/bar
    ("Does roundtrip.", { f = "foo/bar" })
    |}];
  test_fpath (Fpath.v "foo/bar/");
  [%expect
    {|
    foo/bar/
    ("Does roundtrip.", { f = "foo/bar/" })
    |}];
  test_fpath (Fpath.v ".");
  [%expect
    {|
    ./
    ("Does not roundtrip.", { f = "."; f' = "./" })
    |}];
  test_fpath (Fpath.v "./");
  [%expect
    {|
    ./
    ("Does roundtrip.", { f = "./" })
    |}];
  test_fpath (Fpath.v "/an/absolute/path");
  [%expect {| Not a relative path. |}];
  test_fpath (Fpath.v "/an/escaping/absolute/path/../../../../../..");
  [%expect {| Not a relative path. |}];
  require_does_raise (fun () -> test_fpath (Fpath.v "an/../../escaping/path"));
  [%expect
    {| Invalid_argument("Relative_path.of_fpath: path \"an/../../escaping/path\" escapes above starting point") |}];
  ()
;;

let%expect_test "append" =
  let rel = Relative_path.v in
  let test a b = print_endline (Relative_path.append a b |> Relative_path.to_string) in
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
  ()
;;

let%expect_test "extend" =
  let rel = Relative_path.v in
  let file str = str |> Fsegment.v in
  let test a b = print_endline (Relative_path.extend a b |> Relative_path.to_string) in
  require_does_raise (fun () : Fsegment.t -> file "a/b");
  [%expect {| Invalid_argument("Fsegment.v: invalid file segment \"a/b\"") |}];
  ignore (file ".." : Fsegment.t);
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
  test (rel "a/b/") (file "..");
  [%expect {| a/ |}];
  test (rel "a/b") (file "..");
  [%expect {| a/ |}];
  test (rel "a/bar/foo") (file "..");
  [%expect {| a/bar/ |}];
  test (rel "a/") (file "..");
  [%expect {| ./ |}];
  test (rel "a") (file "..");
  [%expect {| ./ |}];
  require_does_raise (fun () -> test (rel "./") (file ".."));
  [%expect
    {| Invalid_argument("Relative_path.extend: path \"./..\" escapes above starting point") |}];
  ()
;;

let%expect_test "parent" =
  let rel = Relative_path.v in
  let test path =
    let result = Relative_path.parent path in
    print_dyn (Dyn.option (fun s -> Dyn.string (s |> Relative_path.to_string)) result)
  in
  test (rel "foo/bar");
  [%expect {| Some "foo/" |}];
  test (rel "foo/bar/");
  [%expect {| Some "foo/" |}];
  test (rel "foo");
  [%expect {| Some "./" |}];
  (* Verify that parent of empty returns [None]. *)
  test Relative_path.empty;
  [%expect {| None |}];
  (* Be specific about the expectations here. *)
  require (Option.is_none (Relative_path.parent Relative_path.empty));
  [%expect {||}];
  ()
;;

let%expect_test "of_list" =
  let test files =
    let result = Relative_path.of_list (List.map files ~f:Fsegment.v) in
    print_endline (result |> Relative_path.to_string)
  in
  test [];
  [%expect {| ./ |}];
  test [ "a" ];
  [%expect {| a |}];
  test [ "." ];
  [%expect {| ./ |}];
  require_does_raise (fun () -> test [ ".." ]);
  [%expect
    {| Invalid_argument("Relative_path.extend: path \"./..\" escapes above starting point") |}];
  require_does_raise (fun () -> test [ "a"; ".."; ".." ]);
  [%expect
    {| Invalid_argument("Relative_path.extend: path \"./..\" escapes above starting point") |}];
  test [ "a"; ".." ];
  [%expect {| ./ |}];
  test [ "a"; "." ];
  [%expect {| a/ |}];
  test [ "a"; "b"; ".."; "c" ];
  [%expect {| a/c |}];
  test [ "a"; "b"; "c"; "d" ];
  [%expect {| a/b/c/d |}];
  (* Even if we would "come back" later, we fail at the intermediate escaping step. *)
  require_does_raise (fun () -> test [ "a"; ".."; ".."; "b"; "c" ]);
  [%expect
    {| Invalid_argument("Relative_path.extend: path \"./..\" escapes above starting point") |}];
  ()
;;

let%expect_test "chop_prefix" =
  let rel = Relative_path.v in
  let test prefix path =
    let result = Relative_path.chop_prefix path ~prefix in
    print_dyn (Dyn.option (fun s -> Dyn.string (s |> Relative_path.to_string)) result)
  in
  test (rel "foo") (rel "foo/bar");
  [%expect {| Some "bar" |}];
  test (rel "foo") (rel "foo/bar/");
  [%expect {| Some "bar/" |}];
  test (rel "foo/") (rel "foo/bar");
  [%expect {| Some "bar" |}];
  test (rel "foo/") (rel "foo/bar/");
  [%expect {| Some "bar/" |}];
  test (rel "foo/") (rel "foo/");
  [%expect {| Some "./" |}];
  test (rel "foo") (rel "foo/");
  [%expect {| Some "./" |}];
  test (rel "foo") (rel "foo");
  [%expect {| Some "./" |}];
  test (rel "foo/") (rel "foo");
  [%expect {| None |}];
  test (rel "foo/.") (rel "foo/");
  [%expect {| Some "./" |}];
  test (rel "foo/.") (rel "foo");
  [%expect {| None |}];
  test (rel "foo/") (rel "foo/.");
  [%expect {| Some "./" |}];
  test (rel "foo") (rel "foo/.");
  [%expect {| Some "./" |}];
  test (rel "foo/bar/") (rel "foo/bar/");
  [%expect {| Some "./" |}];
  test (rel "foo/bar") (rel "foo/bar/");
  [%expect {| Some "./" |}];
  test (rel "foo/bar") (rel "foo/bar");
  [%expect {| Some "./" |}];
  test (rel "foo/bar/") (rel "foo/bar");
  [%expect {| None |}];
  test (rel "foo") (rel "foo/bar/baz");
  [%expect {| Some "bar/baz" |}];
  test (rel "foo") (rel "bar/baz");
  [%expect {| None |}];
  test (rel "foo/bar") (rel "foo");
  [%expect {| None |}];
  test (rel "foo/bar") (rel "foo");
  [%expect {| None |}];
  test (rel "foo/bar") (rel "foo/bar/baz");
  [%expect {| Some "baz" |}];
  test (rel "foo/bar") (rel "foo/bar/baz/qux");
  [%expect {| Some "baz/qux" |}];
  (* Paths are normalized before the function call. *)
  test (rel "foo/bar") (rel "foo/bar/../baz");
  [%expect {| None |}];
  test (rel "foo/bar") (rel "foo/sna/../bar/baz");
  [%expect {| Some "baz" |}];
  (* Beware of string prefix vs path prefix. *)
  test (rel "foo/bar") (rel "foo/bar-baz");
  [%expect {| None |}];
  (* Test that empty prefix returns the input unchanged *)
  test Relative_path.empty (rel "foo/bar");
  [%expect {| Some "foo/bar" |}];
  test Relative_path.empty Relative_path.empty;
  [%expect {| Some "./" |}];
  (* Test directory path behavior: trailing '/' matters for matching. *)
  test (rel "foo/bar/") (rel "foo/bar/baz");
  [%expect {| Some "baz" |}];
  test (rel "foo/bar") (rel "foo/bar/baz");
  [%expect {| Some "baz" |}];
  test (rel "foo/bar/") (rel "foo/bar");
  [%expect {| None |}];
  test (rel "foo/bar") (rel "foo/bar/");
  [%expect {| Some "./" |}];
  (* More complex directory prefix scenarios. *)
  test (rel "a/b/c/") (rel "a/b/c/d/e");
  [%expect {| Some "d/e" |}];
  test (rel "a/b/c") (rel "a/b/c/d/e");
  [%expect {| Some "d/e" |}];
  test (rel "a/b/c/") (rel "a/b/c/d/");
  [%expect {| Some "d/" |}];
  test (rel "a/b/c") (rel "a/b/c/d/");
  [%expect {| Some "d/" |}];
  ()
;;

let%expect_test "chop_suffix" =
  let rel = Relative_path.v in
  let test path suffix =
    let result = Relative_path.chop_suffix path ~suffix in
    print_dyn (Dyn.option (fun s -> Dyn.string (s |> Relative_path.to_string)) result)
  in
  test (rel "foo/bar") (rel "bar");
  [%expect {| Some "foo/" |}];
  test (rel "foo/bar") (rel "bar/");
  [%expect {| None |}];
  test (rel "foo/bar/") (rel "bar");
  [%expect {| None |}];
  test (rel "foo/bar/") (rel "bar/");
  [%expect {| Some "foo/" |}];
  (* Verify that empty suffix returns the input unchanged. *)
  test (rel "foo/bar") Relative_path.empty;
  [%expect {| Some "foo/bar" |}];
  test (rel "foo/bar/") Relative_path.empty;
  [%expect {| Some "foo/bar/" |}];
  test (rel "foo/bar/.") Relative_path.empty;
  [%expect {| Some "foo/bar/" |}];
  test (rel "bar") (rel "foo/bar");
  [%expect {| None |}];
  test (rel "foo/bar") (rel "foo/bar");
  [%expect {| Some "./" |}];
  test (rel "foo/bar") (rel "baz");
  [%expect {| None |}];
  test (rel "foo/bar/baz") (rel "bar/baz");
  [%expect {| Some "foo/" |}];
  test (rel "foo/bar/baz") (rel "baz/qux");
  [%expect {| None |}];
  test (rel "foo/bar/baz") Relative_path.empty;
  [%expect {| Some "foo/bar/baz" |}];
  test (rel "foo/bar/baz") (rel "foo/../baz");
  [%expect {| Some "foo/bar/" |}];
  (* Beware of string suffix vs path suffix. *)
  test (rel "foo/bar-baz") (rel "-baz");
  [%expect {| None |}];
  test Relative_path.empty Relative_path.empty;
  [%expect {| Some "./" |}];
  (* Test directory path behavior: trailing '/' matters for matching. *)
  test (rel "foo/bar") (rel "bar");
  [%expect {| Some "foo/" |}];
  test (rel "foo/bar/") (rel "bar/");
  [%expect {| Some "foo/" |}];
  test (rel "foo/bar/") (rel "bar");
  [%expect {| None |}];
  test (rel "foo/bar") (rel "bar/");
  [%expect {| None |}];
  (* More complex directory suffix scenarios. *)
  test (rel "a/b/c/d/e") (rel "d/e");
  [%expect {| Some "a/b/c/" |}];
  test (rel "a/b/c/d/e/") (rel "d/e/");
  [%expect {| Some "a/b/c/" |}];
  test (rel "a/b/c/d/e") (rel "d/e/");
  [%expect {| None |}];
  test (rel "a/b/c/d/e/") (rel "d/e");
  [%expect {| None |}];
  test (rel "x/y/z/") (rel "z/");
  [%expect {| Some "x/y/" |}];
  test (rel "x/y/z") (rel "z");
  [%expect {| Some "x/y/" |}];
  ()
;;

let%expect_test "is_dir_path" =
  let rel = Relative_path.v in
  let test path = print_dyn (Relative_path.is_dir_path (rel path) |> Dyn.bool) in
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
    print_endline (Relative_path.to_dir_path (rel path) |> Relative_path.to_string)
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
    print_dyn
      (Dyn.Tuple
         [ Record
             [ "path", path |> Relative_path.to_string |> Dyn.string
             ; "is_dir_path", is_dir_path |> Dyn.bool
             ]
         ; Record
             [ "path2", path2 |> Relative_path.to_string |> Dyn.string
             ; "is_dir_path2", is_dir_path2 |> Dyn.bool
             ]
         ])
  in
  test (Relative_path.v "tmp/my-dir/");
  [%expect
    {|
    ({ path = "tmp/my-dir/"; is_dir_path = true },
     { path2 = "tmp/my-dir"; is_dir_path2 = false })
    |}];
  test (Relative_path.v "tmp/my-file");
  [%expect
    {|
    ({ path = "tmp/my-file"; is_dir_path = false },
     { path2 = "tmp/my-file"; is_dir_path2 = false })
    |}];
  ()
;;

let hashtbl_to_dyn key value table =
  let data = Hashtbl.to_alist table in
  Dyn.Map (List.map data ~f:(fun (k, v) -> key k, value v))
;;

let%expect_test "hashtbl" =
  let t = Hashtbl.create (module Relative_path) in
  Hashtbl.set t ~key:(Relative_path.v "path/to/my-file") ~data:42;
  print_dyn (hashtbl_to_dyn (fun p -> Dyn.string (Relative_path.to_string p)) Dyn.int t);
  [%expect {| map { "path/to/my-file" : 42 } |}]
;;

module Pair = struct
  [@@@coverage off]

  type t =
    { a : Relative_path.t
    ; b : Relative_path.t
    }
  [@@deriving compare, hash, sexp_of]

  let to_dyn { a; b } =
    Dyn.Record
      [ "a", a |> Relative_path.to_string |> Dyn.string
      ; "b", b |> Relative_path.to_string |> Dyn.string
      ]
  ;;
end

let%expect_test "hash-fold-t" =
  let t = Hashtbl.create (module Pair) in
  Hashtbl.set
    t
    ~key:{ a = Relative_path.v "path/to/a"; b = Relative_path.v "path/to/b" }
    ~data:42;
  print_dyn (hashtbl_to_dyn Pair.to_dyn Dyn.int t);
  [%expect {| map { { a = "path/to/a"; b = "path/to/b" } : 42 } |}]
;;
