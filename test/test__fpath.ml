let%expect_test "hashtbl" =
  let t = Hashtbl.create (module Fpath) in
  Hashtbl.set t ~key:(Fpath.v "my-file") ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Fpath).t)];
  [%expect {| ((my-file 42)) |}]
;;

module Fpath_pair : sig
  type t =
    { a : Fpath.t
    ; b : Fpath.t
    }
  [@@deriving compare, hash, sexp_of]
end = struct
  type t =
    { a : Fpath.t
    ; b : Fpath.t
    }
  [@@deriving compare, hash, sexp_of]
end

let%expect_test "compare" =
  let test a b =
    print_s
      [%sexp
        (a : Fpath_pair.t)
        , (Fpath_pair.compare a b |> Ordering.of_int : Ordering.t)
        , (b : Fpath_pair.t)]
  in
  test
    { a = Fpath.v "file-a"; b = Fpath.v "file-b" }
    { a = Fpath.v "file-a"; b = Fpath.v "file-b" };
  [%expect
    {|
    (((a file-a)
      (b file-b))
     Equal
     ((a file-a)
      (b file-b))) |}];
  let t = { Fpath_pair.a = Fpath.v "file-a"; b = Fpath.v "file-b" } in
  test t t;
  [%expect
    {|
    (((a file-a)
      (b file-b))
     Equal
     ((a file-a)
      (b file-b))) |}];
  test
    { a = Fpath.v "file-a"; b = Fpath.v "file-a" }
    { a = Fpath.v "file-a"; b = Fpath.v "file-b" };
  [%expect
    {|
    (((a file-a)
      (b file-a))
     Less
     ((a file-a)
      (b file-b))) |}];
  test
    { a = Fpath.v "file-b"; b = Fpath.v "file-a" }
    { a = Fpath.v "file-a"; b = Fpath.v "file-b" };
  [%expect
    {|
    (((a file-b)
      (b file-a))
     Greater
     ((a file-a)
      (b file-b))) |}];
  ()
;;

let%expect_test "hashtbl2" =
  let t = Hashtbl.create (module Fpath_pair) in
  Hashtbl.set t ~key:{ Fpath_pair.a = Fpath.v "file-a"; b = Fpath.v "file-b" } ~data:42;
  print_s [%sexp (t : int Hashtbl.M(Fpath_pair).t)];
  [%expect {|
    ((
      ((a file-a)
       (b file-b))
      42)) |}]
;;

let%expect_test "classify" =
  let test str =
    print_s
      [%sexp
        (Fpath.classify (Fpath.v str)
         : [ `Absolute of Absolute_path.t | `Relative of Relative_path.t ])]
  in
  test "foo/bar";
  [%expect {| (Relative foo/bar) |}];
  test "foo/bar/";
  [%expect {| (Relative foo/bar/) |}];
  test "foo/bar/../baz/../foo/";
  [%expect {| (Relative foo/foo/) |}];
  test "foo";
  [%expect {| (Relative foo) |}];
  test "./";
  [%expect {| (Relative ./) |}];
  test "/foo/bar";
  [%expect {| (Absolute /foo/bar) |}];
  test "/foo/bar/";
  [%expect {| (Absolute /foo/bar/) |}];
  test "/foo/bar/../baz/../foo/";
  [%expect {| (Absolute /foo/foo/) |}];
  test "/foo";
  [%expect {| (Absolute /foo) |}];
  test "/";
  [%expect {| (Absolute /) |}];
  test "/.";
  [%expect {| (Absolute /) |}];
  test ".";
  [%expect {| (Relative ./) |}];
  test "./";
  [%expect {| (Relative ./) |}];
  ()
;;

module Hide_error = struct
  type 'a t = 'a Or_error.t

  let sexp_of_t sexp_of_a : _ t -> Sexp.t = function
    | Ok a -> [%sexp Ok (a : a)]
    | Error _ -> [%sexp Error "_"]
  ;;
end

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
            print_s
              [%sexp
                (a : Absolute_path.t)
                , (b : Absolute_path.t)
                , "==> chop_prefix"
                , (Absolute_path.chop_prefix ~prefix:a b : Relative_path.t Hide_error.t)]
          | `Relative _, `Absolute _ -> assert false
          | `Absolute a, `Relative b ->
            print_s
              [%sexp
                (a : Absolute_path.t)
                , (b : Relative_path.t)
                , "==> chop_suffix"
                , (Absolute_path.chop_suffix a ~suffix:b : Absolute_path.t Hide_error.t)]
          | `Relative a, `Relative b ->
            print_s
              [%sexp
                (a : Relative_path.t)
                , (b : Relative_path.t)
                , "==> chop_prefix"
                , (Relative_path.chop_prefix ~prefix:a b : Relative_path.t Hide_error.t)]))
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
    (/ ./ "==> chop_suffix" (Error _))
    (/ ./ "==> chop_suffix" (Error _))
    (/ ./ "==> chop_suffix" (Error _))
    (/ ./ "==> chop_suffix" (Error _))
    (/ a "==> chop_suffix" (Error _))
    (/ a "==> chop_suffix" (Error _))
    (/ a/ "==> chop_suffix" (Error _))
    (/ a/ "==> chop_suffix" (Error _))
    (/a a "==> chop_suffix" (Ok /))
    (/a/ a "==> chop_suffix" (Error _))
    (/a a/ "==> chop_suffix" (Error _))
    (/a/ a/ "==> chop_suffix" (Ok /))
    (/a b "==> chop_suffix" (Error _))
    (/a/ b "==> chop_suffix" (Error _))
    (/a b/ "==> chop_suffix" (Error _))
    (/a/ b/ "==> chop_suffix" (Error _))
    (/a/b b "==> chop_suffix" (Ok /a))
    (/a/b/ b "==> chop_suffix" (Error _))
    (/a/b b/ "==> chop_suffix" (Error _))
    (/a/b/ b/ "==> chop_suffix" (Ok /a))
    (/a/b a/b "==> chop_suffix" (Ok /))
    (/a/b/ a/b "==> chop_suffix" (Error _))
    (/a/b a/b/ "==> chop_suffix" (Error _))
    (/a/b/ a/b/ "==> chop_suffix" (Ok /))
    (/ / "==> chop_prefix" (Ok ./))
    (/ / "==> chop_prefix" (Ok ./))
    (/ / "==> chop_prefix" (Ok ./))
    (/ / "==> chop_prefix" (Ok ./))
    (/ /a "==> chop_prefix" (Ok a))
    (/ /a "==> chop_prefix" (Ok a))
    (/ /a/ "==> chop_prefix" (Ok a/))
    (/ /a/ "==> chop_prefix" (Ok a/))
    (/a / "==> chop_prefix" (Error _))
    (/a/ / "==> chop_prefix" (Error _))
    (/a / "==> chop_prefix" (Error _))
    (/a/ / "==> chop_prefix" (Error _))
    (/a/b /a/b/c "==> chop_prefix" (Ok c))
    (/a/b/ /a/b/c "==> chop_prefix" (Ok c))
    (/a/b /a/b/c/ "==> chop_prefix" (Ok c/))
    (/a/b/ /a/b/c/ "==> chop_prefix" (Ok c/))
    (/a/b /a/bc "==> chop_prefix" (Error _))
    (/a/b/ /a/bc "==> chop_prefix" (Error _))
    (/a/b /a/bc/ "==> chop_prefix" (Error _))
    (/a/b/ /a/bc/ "==> chop_prefix" (Error _))
    (/a/b /a/b/c/d "==> chop_prefix" (Ok c/d))
    (/a/b/ /a/b/c/d "==> chop_prefix" (Ok c/d))
    (/a/b /a/b/c/d/ "==> chop_prefix" (Ok c/d/))
    (/a/b/ /a/b/c/d/ "==> chop_prefix" (Ok c/d/))
    (/a/b/c /a/b/c "==> chop_prefix" (Ok ./))
    (/a/b/c/ /a/b/c "==> chop_prefix" (Error _))
    (/a/b/c /a/b/c/ "==> chop_prefix" (Ok ./))
    (/a/b/c/ /a/b/c/ "==> chop_prefix" (Ok ./))
    (/a/b/c /a/b "==> chop_prefix" (Error _))
    (/a/b/c/ /a/b "==> chop_prefix" (Error _))
    (/a/b/c /a/b/ "==> chop_prefix" (Error _))
    (/a/b/c/ /a/b/ "==> chop_prefix" (Error _))
    (./ ./ "==> chop_prefix" (Ok ./))
    (./ ./ "==> chop_prefix" (Ok ./))
    (./ ./ "==> chop_prefix" (Ok ./))
    (./ ./ "==> chop_prefix" (Ok ./))
    (./ a "==> chop_prefix" (Error _))
    (./ a "==> chop_prefix" (Error _))
    (./ a/ "==> chop_prefix" (Error _))
    (./ a/ "==> chop_prefix" (Error _))
    (a ./ "==> chop_prefix" (Error _))
    (a/ ./ "==> chop_prefix" (Error _))
    (a ./ "==> chop_prefix" (Error _))
    (a/ ./ "==> chop_prefix" (Error _))
    (a/b a/b/c "==> chop_prefix" (Ok c))
    (a/b/ a/b/c "==> chop_prefix" (Ok c))
    (a/b a/b/c/ "==> chop_prefix" (Ok c/))
    (a/b/ a/b/c/ "==> chop_prefix" (Ok c/))
    (a/b a/bc "==> chop_prefix" (Error _))
    (a/b/ a/bc "==> chop_prefix" (Error _))
    (a/b a/bc/ "==> chop_prefix" (Error _))
    (a/b/ a/bc/ "==> chop_prefix" (Error _))
    (a/b a/b/c/d "==> chop_prefix" (Ok c/d))
    (a/b/ a/b/c/d "==> chop_prefix" (Ok c/d))
    (a/b a/b/c/d/ "==> chop_prefix" (Ok c/d/))
    (a/b/ a/b/c/d/ "==> chop_prefix" (Ok c/d/))
    (a/b/c a/b/c "==> chop_prefix" (Ok ./))
    (a/b/c/ a/b/c "==> chop_prefix" (Error _))
    (a/b/c a/b/c/ "==> chop_prefix" (Ok ./))
    (a/b/c/ a/b/c/ "==> chop_prefix" (Ok ./))
    (a/b/c a/b "==> chop_prefix" (Error _))
    (a/b/c/ a/b "==> chop_prefix" (Error _))
    (a/b/c a/b/ "==> chop_prefix" (Error _))
    (a/b/c/ a/b/ "==> chop_prefix" (Error _)) |}];
  ()
;;
