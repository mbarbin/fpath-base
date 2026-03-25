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
   escaping paths:

   @mdexp.code *)
let%expect_test "construction rejects escaping" =
  (match Relative_path.v "../config" with
   | (_ : Relative_path.t) -> assert false
   | exception Invalid_argument msg -> print_endline msg);
  [%expect {| Relative_path.v: path "../config" escapes above starting point |}]
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

   Returns `None` for the empty path (previously returned `"../"`):

   @mdexp.code *)
let%expect_test "parent of empty" =
  print_string
    (match Relative_path.parent Relative_path.empty with
     | None -> "None"
     | Some _ -> assert false);
  [%expect {| None |}]
;;

(* @mdexp

   This fixes infinite loops in upward navigation:

   @mdexp.code *)
let%expect_test "navigate to root" =
  let rec navigate_to_root path =
    match Relative_path.parent path with
    | None -> path
    | Some p -> navigate_to_root p
  in
  print_string (Relative_path.to_string (navigate_to_root (Relative_path.v "a/b/c")));
  [%expect {| ./ |}]
;;

(* @mdexp

   ### Extend Function

   Raises `Invalid_argument` if extending creates an escaping path:

   @mdexp.code *)
let%expect_test "extend rejects escaping" =
  (match Relative_path.extend Relative_path.empty (Fsegment.v "..") with
   | (_ : Relative_path.t) -> assert false
   | exception Invalid_argument msg -> print_endline msg);
  [%expect {| Relative_path.extend: path "./.." escapes above starting point |}]
;;

(* @mdexp

   **Migration:** Use `Fpath.t` if segments might create escaping paths.

   ### Chop Prefix/Suffix

   Empty prefix/suffix now returns `Some path` (previously `None`):

   @mdexp.code *)
let%expect_test "chop prefix with empty" =
  print_string
    (match
       Relative_path.chop_prefix (Relative_path.v "foo/bar") ~prefix:Relative_path.empty
     with
     | None -> assert false
     | Some p -> Relative_path.to_string p);
  [%expect {| foo/bar |}]
;;

(* @mdexp

   ## Common Migration Patterns

   ### Dynamic Path Construction

   Validate paths and handle rejections:

   @mdexp.code *)
let try_load filename =
  match Relative_path.of_string filename with
  | Ok p -> Printf.printf "%s => Ok %s\n" filename (Relative_path.to_string p)
  | Error (`Msg msg) -> Printf.printf "%s => Error: %s\n" filename msg
;;

(* @mdexp.end *)

(* @mdexp.code *)
let%expect_test "validate user input" =
  try_load "config/settings.conf";
  [%expect {| config/settings.conf => Ok config/settings.conf |}];
  try_load "../../../etc/passwd";
  [%expect
    {| ../../../etc/passwd => Error: path "../../../etc/passwd" escapes above starting point |}]
;;

(* @mdexp

   ### Upward Navigation

   Use absolute paths for upward traversal:

   @mdexp.code *)

let find_project_root ~has_marker current_path =
  let rec search path =
    if has_marker path
    then Some path
    else (
      match Absolute_path.parent path with
      | None -> None
      | Some parent -> search parent)
  in
  search current_path
;;

(* @mdexp.end *)

let _ = find_project_root

(* @mdexp

   ## Why These Changes

   1. **Improve type safety** - `Relative_path.t` guarantees non-escaping
   2. **Less error-prone APIs** for sandbox operations and recursive parent
   traversal

   See [Path Normalization](../explanation/path-normalization.md) for more
   details. *)
