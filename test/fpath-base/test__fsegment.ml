(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

let%expect_test "sexp_of_t" =
  print_endline
    (Sexplib0.Sexp.to_string_hum (Fsegment.sexp_of_t (Fsegment.v "hello-segment")));
  [%expect {| hello-segment |}];
  ()
;;

let%expect_test "of_string" =
  let test str =
    print_dyn
      (or_msg_to_dyn
         (fun p -> Dyn.string (Fsegment.to_string p))
         (Fsegment.of_string str))
  in
  test "";
  [%expect {| Error (Msg "invalid file segment \"\"") |}];
  test "a";
  [%expect {| Ok "a" |}];
  test ".a";
  [%expect {| Ok ".a" |}];
  test "..";
  [%expect {| Ok ".." |}];
  test "/";
  [%expect {| Error (Msg "invalid file segment \"/\"") |}];
  test "a/b";
  [%expect {| Error (Msg "invalid file segment \"a/b\"") |}];
  test "a\000b";
  [%expect {| Error (Msg "invalid file segment \"a\\000b\"") |}];
  ()
;;

let%expect_test "hard coded" =
  List.iter
    ~f:(fun (name, t) ->
      print_endline (Printf.sprintf "%10s: " name ^ Fsegment.to_string t))
    Fsegment.[ "dot", dot; "dot_dot", dot_dot; "dot_git", dot_git; "dot_hg", dot_hg ];
  [%expect
    {|
        dot: .
    dot_dot: ..
    dot_git: .git
     dot_hg: .hg |}];
  ()
;;

let hashtbl_to_dyn key value table =
  let data = Hashtbl.to_alist table in
  Dyn.Map (List.map data ~f:(fun (k, v) -> key k, value v))
;;

let%expect_test "hashtbl" =
  let t = Hashtbl.create (module Fsegment) in
  Hashtbl.set t ~key:(Fsegment.v "my-file") ~data:42;
  print_dyn (hashtbl_to_dyn (fun p -> Dyn.string (Fsegment.to_string p)) Dyn.int t);
  [%expect {| map { "my-file" : 42 } |}]
;;

module Pair = struct
  [@@@coverage off]

  type t =
    { a : Fsegment.t
    ; b : Fsegment.t
    }
  [@@deriving compare, hash, sexp_of]

  let to_dyn { a; b } =
    Dyn.Record
      [ "a", a |> Fsegment.to_string |> Dyn.string
      ; "b", b |> Fsegment.to_string |> Dyn.string
      ]
  ;;
end

let%expect_test "hash-fold-t" =
  let t = Hashtbl.create (module Pair) in
  Hashtbl.set t ~key:{ a = Fsegment.v "a"; b = Fsegment.v "b" } ~data:42;
  print_dyn (hashtbl_to_dyn Pair.to_dyn Dyn.int t);
  [%expect {| map { { a = "a"; b = "b" } : 42 } |}]
;;
