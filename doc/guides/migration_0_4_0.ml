(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

(* @mdexp

   # Migration Guide: Version 0.4.0

   ## Summary

   Version 0.4.0 makes `Relative_path.t` reject paths that escape above their
   starting point (paths with leading `..` segments after normalization).

   ## Breaking Changes

   ### Construction Functions

   All construction functions (`v`, `of_string`, `of_fpath`) now reject
   escaping paths: *)

let%expect_test "construction rejects escaping" =
  (try ignore (Relative_path.v "../config" : Relative_path.t) with
   | Invalid_argument msg ->
     Printf.printf "Relative_path.v \"../config\" raises:\nInvalid_argument %S\n" msg);
  (* @mdexp.snapshot { lang: "ocaml" } *)
  [%expect
    {|
    Relative_path.v "../config" raises:
    Invalid_argument "Relative_path.v: path \"../config\" escapes above starting point"
    |}]
;;

(* @mdexp

   **Migration options:**

   Use `Absolute_path.t` for explicit paths:

   ```ocaml
   let path = Absolute_path.v "/path/to/parent/config"
   ```

   Or use `Fpath.t` for paths that may escape:

   ```ocaml
   let path : Fpath.t = Fpath.v "../config" |> Fpath.normalize
   ```

   ### Parent Function

   Returns `None` for the empty path (previously returned `"../"`): *)

let%expect_test "parent of empty" =
  Printf.printf
    "Relative_path.parent Relative_path.empty => %s\n"
    (match Relative_path.parent Relative_path.empty with
     | None -> "None"
     | Some p -> Printf.sprintf "Some %S" (Relative_path.to_string p));
  (* @mdexp.snapshot { lang: "ocaml" } *)
  [%expect {| Relative_path.parent Relative_path.empty => None |}]
;;

(* @mdexp

   This fixes infinite loops in upward navigation: *)

let%expect_test "navigate to root" =
  let rec navigate_to_root path =
    match Relative_path.parent path with
    | None -> path
    | Some p -> navigate_to_root p
  in
  Printf.printf
    "navigate_to_root (Relative_path.v \"a/b/c\") => %S\n"
    (Relative_path.to_string (navigate_to_root (Relative_path.v "a/b/c")));
  (* @mdexp.snapshot { lang: "ocaml" } *)
  [%expect {| navigate_to_root (Relative_path.v "a/b/c") => "./" |}]
;;

(* @mdexp

   ### Extend Function

   Raises `Invalid_argument` if extending creates an escaping path: *)

let%expect_test "extend rejects escaping" =
  (try
     ignore (Relative_path.extend Relative_path.empty (Fsegment.v "..") : Relative_path.t)
   with
   | Invalid_argument msg ->
     Printf.printf
       "Relative_path.extend Relative_path.empty (Fsegment.v \"..\") raises:\n\
        Invalid_argument %S\n"
       msg);
  (* @mdexp.snapshot { lang: "ocaml" } *)
  [%expect
    {|
    Relative_path.extend Relative_path.empty (Fsegment.v "..") raises:
    Invalid_argument "Relative_path.extend: path \"./..\" escapes above starting point"
    |}]
;;

(* @mdexp

   **Migration:** Use `Fpath.t` if segments might create escaping paths.

   ### Chop Prefix/Suffix

   Empty prefix/suffix now returns `Some path` (previously `None`): *)

let%expect_test "chop prefix with empty" =
  let result =
    match
      Relative_path.chop_prefix (Relative_path.v "foo/bar") ~prefix:Relative_path.empty
    with
    | None -> "no match"
    | Some p -> Relative_path.to_string p
  in
  Printf.printf "chop_prefix (Relative_path.v \"foo/bar\") ~prefix:empty => %S\n" result;
  (* @mdexp.snapshot { lang: "ocaml" } *)
  [%expect {| chop_prefix (Relative_path.v "foo/bar") ~prefix:empty => "foo/bar" |}]
;;

(* @mdexp

   ## Common Migration Patterns

   ### Dynamic Path Construction

   Validate paths and handle rejections: *)

let%expect_test "dynamic path construction" =
  let load_relative_file filename =
    match Relative_path.of_string filename with
    | Error (`Msg err) -> Error err
    | Ok path -> Ok path
  in
  (match load_relative_file "config/settings.conf" with
   | Ok p ->
     Printf.printf
       "load_relative_file \"config/settings.conf\" => Ok %S\n"
       (Relative_path.to_string p)
   | Error err ->
     Printf.printf "load_relative_file \"config/settings.conf\" => Error %S\n" err);
  (* @mdexp.snapshot { lang: "ocaml" } *)
  [%expect
    {|
    load_relative_file "config/settings.conf" => Ok "config/settings.conf"
    |}]
;;

(* @mdexp

   ### Upward Navigation

   Use absolute paths for upward traversal:

   ```ocaml
   let find_project_root has_marker current_path =
     let rec search path =
       if has_marker path then Some path
       else
         match Absolute_path.parent path with
         | None -> None
         | Some parent -> search parent
     in
     search current_path
   ```

   ## Why These Changes

   1. **Improve type safety** - `Relative_path.t` guarantees non-escaping
   2. **Less error-prone APIs** for sandbox operations and recursive parent
   traversal

   See [Path Normalization](../explanation/path-normalization.md) for more
   details. *)
