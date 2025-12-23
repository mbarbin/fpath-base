(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

let%expect_test "sexp_of_t" =
  print_endline
    (Sexplib0.Sexp.to_string_hum (Fpath.sexp_of_t (Fpath.v "hello/path/to/sexp")));
  [%expect {| hello/path/to/sexp |}];
  ()
;;

let hashtbl_to_dyn key value table =
  let data = Hashtbl.to_alist table in
  Dyn.Map (List.map data ~f:(fun (k, v) -> key k, value v))
;;

let%expect_test "hashtbl" =
  let t = Hashtbl.create (module Fpath) in
  Hashtbl.set t ~key:(Fpath.v "my-file") ~data:42;
  print_dyn (hashtbl_to_dyn (fun p -> Dyn.string (Fpath.to_string p)) Dyn.int t);
  [%expect {| map { "my-file" : 42 } |}]
;;

module Fpath_pair : sig
  type t =
    { a : Fpath.t
    ; b : Fpath.t
    }
  [@@deriving compare, hash, sexp_of]

  val to_dyn : t -> Dyn.t
end = struct
  include struct
    [@@@coverage off]

    type t =
      { a : Fpath.t
      ; b : Fpath.t
      }
    [@@deriving compare, hash, sexp_of]
  end

  let to_dyn { a; b } =
    Dyn.Record
      [ "a", a |> Fpath.to_string |> Dyn.string; "b", b |> Fpath.to_string |> Dyn.string ]
  ;;
end

let%expect_test "compare" =
  let test a b =
    print_dyn
      (Dyn.Tuple
         [ a |> Fpath_pair.to_dyn
         ; Fpath_pair.compare a b |> Ordering.of_int |> Ordering.to_dyn
         ; b |> Fpath_pair.to_dyn
         ])
  in
  test
    { a = Fpath.v "file-a"; b = Fpath.v "file-b" }
    { a = Fpath.v "file-a"; b = Fpath.v "file-b" };
  [%expect {| ({ a = "file-a"; b = "file-b" }, Eq, { a = "file-a"; b = "file-b" }) |}];
  let t = { Fpath_pair.a = Fpath.v "file-a"; b = Fpath.v "file-b" } in
  test t t;
  [%expect {| ({ a = "file-a"; b = "file-b" }, Eq, { a = "file-a"; b = "file-b" }) |}];
  test
    { a = Fpath.v "file-a"; b = Fpath.v "file-a" }
    { a = Fpath.v "file-a"; b = Fpath.v "file-b" };
  [%expect {| ({ a = "file-a"; b = "file-a" }, Lt, { a = "file-a"; b = "file-b" }) |}];
  test
    { a = Fpath.v "file-b"; b = Fpath.v "file-a" }
    { a = Fpath.v "file-a"; b = Fpath.v "file-b" };
  [%expect {| ({ a = "file-b"; b = "file-a" }, Gt, { a = "file-a"; b = "file-b" }) |}];
  ()
;;

let%expect_test "hashtbl2" =
  let t = Hashtbl.create (module Fpath_pair) in
  Hashtbl.set t ~key:{ Fpath_pair.a = Fpath.v "file-a"; b = Fpath.v "file-b" } ~data:42;
  print_dyn (hashtbl_to_dyn Fpath_pair.to_dyn Dyn.int t);
  [%expect {| map { { a = "file-a"; b = "file-b" } : 42 } |}]
;;

let%expect_test "classify" =
  let test str =
    match Fpath.classify (Fpath.v str) with
    | `Absolute path ->
      print_dyn (Dyn.Variant ("Absolute", [ Dyn.String (Absolute_path.to_string path) ]))
    | `Relative path ->
      print_dyn (Dyn.Variant ("Relative", [ Dyn.String (Relative_path.to_string path) ]))
  in
  test "foo/bar";
  [%expect {| Relative "foo/bar" |}];
  test "foo/bar/";
  [%expect {| Relative "foo/bar/" |}];
  test "foo/bar/../baz/../foo/";
  [%expect {| Relative "foo/foo/" |}];
  test "foo";
  [%expect {| Relative "foo" |}];
  test "./";
  [%expect {| Relative "./" |}];
  test "/foo/bar";
  [%expect {| Absolute "/foo/bar" |}];
  test "/foo/bar/";
  [%expect {| Absolute "/foo/bar/" |}];
  test "/foo/bar/../baz/../foo/";
  [%expect {| Absolute "/foo/foo/" |}];
  test "/foo";
  [%expect {| Absolute "/foo" |}];
  test "/";
  [%expect {| Absolute "/" |}];
  test "/.";
  [%expect {| Absolute "/" |}];
  test ".";
  [%expect {| Relative "./" |}];
  test "./";
  [%expect {| Relative "./" |}];
  test "/a/b/../../../..";
  [%expect {| Absolute "/" |}];
  test "/a/b/../../../../foo/";
  [%expect {| Absolute "/foo/" |}];
  require_does_raise (fun () -> test "a/b/../../../..");
  [%expect
    {| Invalid_argument("Fpath.classify: path \"a/b/../../../..\" escapes above starting point") |}];
  ()
;;

let%expect_test "chop_prefix and chop_suffix" =
  List.iter
    ~f:(fun (a, b) ->
      let a, b = Fpath.v a, Fpath.v b in
      List.iter
        [ a, b
        ; Fpath.to_dir_path a, b
        ; a, Fpath.to_dir_path b
        ; Fpath.to_dir_path a, Fpath.to_dir_path b
        ]
        ~f:(fun (a, b) ->
          match Fpath.classify a, Fpath.classify b with
          | `Absolute a, `Absolute b ->
            print_dyn
              (Dyn.Tuple
                 [ a |> Absolute_path.to_string |> Dyn.string
                 ; b |> Absolute_path.to_string |> Dyn.string
                 ; String "==> chop_prefix"
                 ; Absolute_path.chop_prefix b ~prefix:a
                   |> Dyn.option (fun s -> Dyn.string (Relative_path.to_string s))
                 ])
          | `Relative _, `Absolute _ -> assert false
          | `Absolute a, `Relative b ->
            print_dyn
              (Dyn.Tuple
                 [ a |> Absolute_path.to_string |> Dyn.string
                 ; b |> Relative_path.to_string |> Dyn.string
                 ; String "==> chop_suffix"
                 ; Absolute_path.chop_suffix a ~suffix:b
                   |> Dyn.option (fun s -> Dyn.string (Absolute_path.to_string s))
                 ])
          | `Relative a, `Relative b ->
            print_dyn
              (Dyn.Tuple
                 [ a |> Relative_path.to_string |> Dyn.string
                 ; b |> Relative_path.to_string |> Dyn.string
                 ; String "==> chop_prefix"
                 ; Relative_path.chop_prefix b ~prefix:a
                   |> Dyn.option (fun s -> Dyn.string (Relative_path.to_string s))
                 ])))
    [ "/", "."
    ; "/", "a"
    ; "/a", "a"
    ; "/a", "b"
    ; "/a/b", "b"
    ; "/a/b", "a/b"
    ; "/", "/"
    ; "/", "/a"
    ; "/a", "/"
    ; "/a/b", "/a/b/c"
    ; "/a/b", "/a/bc"
    ; "/a/b", "/a/b/c/d"
    ; "/a/b/c", "/a/b/c"
    ; "/a/b/c", "/a/b"
    ; ".", "."
    ; ".", "a"
    ; "a", "."
    ; "a/b", "a/b/c"
    ; "a/b", "a/bc"
    ; "a/b", "a/b/c/d"
    ; "a/b/c", "a/b/c"
    ; "a/b/c", "a/b"
    ];
  [%expect
    {|
    ("/", "./", "==> chop_suffix", Some "/")
    ("/", "./", "==> chop_suffix", Some "/")
    ("/", "./", "==> chop_suffix", Some "/")
    ("/", "./", "==> chop_suffix", Some "/")
    ("/", "a", "==> chop_suffix", None)
    ("/", "a", "==> chop_suffix", None)
    ("/", "a/", "==> chop_suffix", None)
    ("/", "a/", "==> chop_suffix", None)
    ("/a", "a", "==> chop_suffix", Some "/")
    ("/a/", "a", "==> chop_suffix", None)
    ("/a", "a/", "==> chop_suffix", None)
    ("/a/", "a/", "==> chop_suffix", Some "/")
    ("/a", "b", "==> chop_suffix", None)
    ("/a/", "b", "==> chop_suffix", None)
    ("/a", "b/", "==> chop_suffix", None)
    ("/a/", "b/", "==> chop_suffix", None)
    ("/a/b", "b", "==> chop_suffix", Some "/a/")
    ("/a/b/", "b", "==> chop_suffix", None)
    ("/a/b", "b/", "==> chop_suffix", None)
    ("/a/b/", "b/", "==> chop_suffix", Some "/a/")
    ("/a/b", "a/b", "==> chop_suffix", Some "/")
    ("/a/b/", "a/b", "==> chop_suffix", None)
    ("/a/b", "a/b/", "==> chop_suffix", None)
    ("/a/b/", "a/b/", "==> chop_suffix", Some "/")
    ("/", "/", "==> chop_prefix", Some "./")
    ("/", "/", "==> chop_prefix", Some "./")
    ("/", "/", "==> chop_prefix", Some "./")
    ("/", "/", "==> chop_prefix", Some "./")
    ("/", "/a", "==> chop_prefix", Some "a")
    ("/", "/a", "==> chop_prefix", Some "a")
    ("/", "/a/", "==> chop_prefix", Some "a/")
    ("/", "/a/", "==> chop_prefix", Some "a/")
    ("/a", "/", "==> chop_prefix", None)
    ("/a/", "/", "==> chop_prefix", None)
    ("/a", "/", "==> chop_prefix", None)
    ("/a/", "/", "==> chop_prefix", None)
    ("/a/b", "/a/b/c", "==> chop_prefix", Some "c")
    ("/a/b/", "/a/b/c", "==> chop_prefix", Some "c")
    ("/a/b", "/a/b/c/", "==> chop_prefix", Some "c/")
    ("/a/b/", "/a/b/c/", "==> chop_prefix", Some "c/")
    ("/a/b", "/a/bc", "==> chop_prefix", None)
    ("/a/b/", "/a/bc", "==> chop_prefix", None)
    ("/a/b", "/a/bc/", "==> chop_prefix", None)
    ("/a/b/", "/a/bc/", "==> chop_prefix", None)
    ("/a/b", "/a/b/c/d", "==> chop_prefix", Some "c/d")
    ("/a/b/", "/a/b/c/d", "==> chop_prefix", Some "c/d")
    ("/a/b", "/a/b/c/d/", "==> chop_prefix", Some "c/d/")
    ("/a/b/", "/a/b/c/d/", "==> chop_prefix", Some "c/d/")
    ("/a/b/c", "/a/b/c", "==> chop_prefix", Some "./")
    ("/a/b/c/", "/a/b/c", "==> chop_prefix", None)
    ("/a/b/c", "/a/b/c/", "==> chop_prefix", Some "./")
    ("/a/b/c/", "/a/b/c/", "==> chop_prefix", Some "./")
    ("/a/b/c", "/a/b", "==> chop_prefix", None)
    ("/a/b/c/", "/a/b", "==> chop_prefix", None)
    ("/a/b/c", "/a/b/", "==> chop_prefix", None)
    ("/a/b/c/", "/a/b/", "==> chop_prefix", None)
    ("./", "./", "==> chop_prefix", Some "./")
    ("./", "./", "==> chop_prefix", Some "./")
    ("./", "./", "==> chop_prefix", Some "./")
    ("./", "./", "==> chop_prefix", Some "./")
    ("./", "a", "==> chop_prefix", Some "a")
    ("./", "a", "==> chop_prefix", Some "a")
    ("./", "a/", "==> chop_prefix", Some "a/")
    ("./", "a/", "==> chop_prefix", Some "a/")
    ("a", "./", "==> chop_prefix", None)
    ("a/", "./", "==> chop_prefix", None)
    ("a", "./", "==> chop_prefix", None)
    ("a/", "./", "==> chop_prefix", None)
    ("a/b", "a/b/c", "==> chop_prefix", Some "c")
    ("a/b/", "a/b/c", "==> chop_prefix", Some "c")
    ("a/b", "a/b/c/", "==> chop_prefix", Some "c/")
    ("a/b/", "a/b/c/", "==> chop_prefix", Some "c/")
    ("a/b", "a/bc", "==> chop_prefix", None)
    ("a/b/", "a/bc", "==> chop_prefix", None)
    ("a/b", "a/bc/", "==> chop_prefix", None)
    ("a/b/", "a/bc/", "==> chop_prefix", None)
    ("a/b", "a/b/c/d", "==> chop_prefix", Some "c/d")
    ("a/b/", "a/b/c/d", "==> chop_prefix", Some "c/d")
    ("a/b", "a/b/c/d/", "==> chop_prefix", Some "c/d/")
    ("a/b/", "a/b/c/d/", "==> chop_prefix", Some "c/d/")
    ("a/b/c", "a/b/c", "==> chop_prefix", Some "./")
    ("a/b/c/", "a/b/c", "==> chop_prefix", None)
    ("a/b/c", "a/b/c/", "==> chop_prefix", Some "./")
    ("a/b/c/", "a/b/c/", "==> chop_prefix", Some "./")
    ("a/b/c", "a/b", "==> chop_prefix", None)
    ("a/b/c/", "a/b", "==> chop_prefix", None)
    ("a/b/c", "a/b/", "==> chop_prefix", None)
    ("a/b/c/", "a/b/", "==> chop_prefix", None)
    |}];
  ()
;;
