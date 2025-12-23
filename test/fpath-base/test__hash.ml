(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

(* Below we verify that the modules are compatible with stdlib hash functors. *)

module _ = Stdlib.MoreLabels.Hashtbl.Make (Fpath_sexp0.Fpath)
module _ = Stdlib.MoreLabels.Hashtbl.MakeSeeded (Fpath_sexp0.Fpath)
module _ = Stdlib.MoreLabels.Hashtbl.Make (Fpath_sexp0.Fsegment)
module _ = Stdlib.MoreLabels.Hashtbl.MakeSeeded (Fpath_sexp0.Fsegment)
module _ = Stdlib.MoreLabels.Hashtbl.Make (Fpath_sexp0.Absolute_path)
module _ = Stdlib.MoreLabels.Hashtbl.MakeSeeded (Fpath_sexp0.Absolute_path)
module _ = Stdlib.MoreLabels.Hashtbl.Make (Fpath_sexp0.Relative_path)
module _ = Stdlib.MoreLabels.Hashtbl.MakeSeeded (Fpath_sexp0.Relative_path)

(* If you are using [Fpath_base] we expect that you'll be using base style
   containers, however the seeded hash functions are exposed too. *)

module _ = Stdlib.MoreLabels.Hashtbl.Make (Fpath_base.Fpath)
module _ = Stdlib.MoreLabels.Hashtbl.MakeSeeded (Fpath_base.Fpath)
module _ = Stdlib.MoreLabels.Hashtbl.Make (Fpath_base.Fsegment)
module _ = Stdlib.MoreLabels.Hashtbl.MakeSeeded (Fpath_base.Fsegment)
module _ = Stdlib.MoreLabels.Hashtbl.Make (Fpath_base.Absolute_path)
module _ = Stdlib.MoreLabels.Hashtbl.MakeSeeded (Fpath_base.Absolute_path)
module _ = Stdlib.MoreLabels.Hashtbl.Make (Fpath_base.Relative_path)
module _ = Stdlib.MoreLabels.Hashtbl.MakeSeeded (Fpath_base.Relative_path)

let%expect_test "hash" =
  let seg = Fsegment.v "file" in
  let h1 = Fpath_base.Fsegment.hash seg in
  let h2 = Fpath_sexp0.Fsegment.hash seg in
  print_dyn (h1 |> Dyn.int);
  [%expect {| 437367475 |}];
  require_equal [%here] (module Int) h1 h2;
  [%expect {||}];
  ()
;;

let%expect_test "Fsegment.seeded_hash" =
  let seg = Fsegment.v "file" in
  let s0 = Fsegment.seeded_hash 0 seg in
  let s42 = Fsegment.seeded_hash 42 seg in
  print_dyn (s0 |> Dyn.int);
  [%expect {| 437367475 |}];
  print_dyn (s42 |> Dyn.int);
  [%expect {| 202913284 |}];
  let f0 = Fsegment.hash_fold_t (Hash.create ()) seg |> Hash.get_hash_value in
  let f42 = Fsegment.hash_fold_t (Hash.create ~seed:42 ()) seg |> Hash.get_hash_value in
  print_dyn (f0 |> Dyn.int);
  [%expect {| 437367475 |}];
  print_dyn (f42 |> Dyn.int);
  [%expect {| 202913284 |}];
  require_equal [%here] (module Int) s0 f0;
  require_equal [%here] (module Int) s42 f42;
  [%expect {||}];
  ()
;;

let%expect_test "Fpath.seeded_hash" =
  let path = Fpath.v "file" in
  let s0 = Fpath.seeded_hash 0 path in
  let s42 = Fpath.seeded_hash 42 path in
  print_dyn (s0 |> Dyn.int);
  [%expect {| 437367475 |}];
  print_dyn (s42 |> Dyn.int);
  [%expect {| 202913284 |}];
  let f0 = Fpath.hash_fold_t (Hash.create ()) path |> Hash.get_hash_value in
  let f42 = Fpath.hash_fold_t (Hash.create ~seed:42 ()) path |> Hash.get_hash_value in
  print_dyn (f0 |> Dyn.int);
  [%expect {| 437367475 |}];
  print_dyn (f42 |> Dyn.int);
  [%expect {| 202913284 |}];
  require_equal [%here] (module Int) s0 f0;
  require_equal [%here] (module Int) s42 f42;
  [%expect {||}];
  ()
;;

let%expect_test "Relative_path.seeded_hash" =
  let path = Relative_path.v "file" in
  let s0 = Relative_path.seeded_hash 0 path in
  let s42 = Relative_path.seeded_hash 42 path in
  print_dyn (s0 |> Dyn.int);
  [%expect {| 437367475 |}];
  print_dyn (s42 |> Dyn.int);
  [%expect {| 202913284 |}];
  let f0 = Relative_path.hash_fold_t (Hash.create ()) path |> Hash.get_hash_value in
  let f42 =
    Relative_path.hash_fold_t (Hash.create ~seed:42 ()) path |> Hash.get_hash_value
  in
  print_dyn (f0 |> Dyn.int);
  [%expect {| 437367475 |}];
  print_dyn (f42 |> Dyn.int);
  [%expect {| 202913284 |}];
  require_equal [%here] (module Int) s0 f0;
  require_equal [%here] (module Int) s42 f42;
  [%expect {||}];
  ()
;;

let%expect_test "Absolute_path.seeded_hash" =
  let path = Absolute_path.v "/tmp/my-file" in
  let s0 = Absolute_path.seeded_hash 0 path in
  let s42 = Absolute_path.seeded_hash 42 path in
  print_dyn (s0 |> Dyn.int);
  [%expect {| 152999615 |}];
  print_dyn (s42 |> Dyn.int);
  [%expect {| 524647462 |}];
  let f0 = Absolute_path.hash_fold_t (Hash.create ()) path |> Hash.get_hash_value in
  let f42 =
    Absolute_path.hash_fold_t (Hash.create ~seed:42 ()) path |> Hash.get_hash_value
  in
  print_dyn (f0 |> Dyn.int);
  [%expect {| 152999615 |}];
  print_dyn (f42 |> Dyn.int);
  [%expect {| 524647462 |}];
  require_equal [%here] (module Int) s0 f0;
  require_equal [%here] (module Int) s42 f42;
  [%expect {||}];
  ()
;;
