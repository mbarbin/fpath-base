(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

(* @mdexp

   # Path Normalization and Escaping Prevention

   ## Overview

   Starting in version 0.4.0, `Relative_path.t` rejects paths that escape
   above their starting point.

   **The process:**
   1. Paths are normalized using `Fpath.normalize` (resolves `.` and `..`
   segments)
   2. If the normalized path has leading `..` segments, it's rejected by
   `Relative_path.t`

   ## What Gets Rejected

   Paths that escape above their starting point: *)

let%expect_test "rejected paths" =
  let show str =
    Printf.printf "Relative_path.v %S\n" str;
    match Relative_path.of_string str with
    | Ok _ -> print_endline "=> Ok"
    | Error (`Msg msg) -> Printf.printf "=> Error: %s\n\n" msg
  in
  show "..";
  show "../config";
  show "a/../..";
  (* @mdexp.snapshot { lang: "ocaml" } *)
  [%expect
    {|
    Relative_path.v ".."
    => Error: path ".." escapes above starting point

    Relative_path.v "../config"
    => Error: path "../config" escapes above starting point

    Relative_path.v "a/../.."
    => Error: path "a/../.." escapes above starting point
    |}]
;;

(* @mdexp

   Paths that stay within bounds are accepted: *)

let%expect_test "accepted paths" =
  let show str =
    Printf.printf
      "Relative_path.v %S => %S\n"
      str
      (Relative_path.to_string (Relative_path.v str))
  in
  show "a/..";
  show "a/b/../c";
  (* @mdexp.snapshot { lang: "ocaml" } *)
  [%expect
    {|
    Relative_path.v "a/.." => "./"
    Relative_path.v "a/b/../c" => "a/c"
    |}]
;;

(* @mdexp

   ## Why This Matters

   ### Prevents Memory Growth

   Before v0.4.0, calling `parent` repeatedly could grow memory unboundedly.
   Starting from `"./"`:
   - `parent "./"` returned `"../"`
   - `parent "../"` returned `"../../"`
   - `parent "../../"` returned `"../../../"` — and so on forever

   After v0.4.0: *)

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

   ### Type Safety Guarantee

   `Relative_path.t` now guarantees the path won't escape above its starting
   point, making it safe for:
   - Sandbox operations (can't escape sandbox root)
   - Archive extraction (can't write outside target directory)
   - Path concatenation (stays within base directory)

   ## When You Need Escaping Paths

   If you need paths with leading `..` segments, use `Fpath.t` directly:

   ```ocaml
   let path : Fpath.t = Fpath.v "../config" |> Fpath.normalize
   ```

   ## Type Selection Guide

   ```
   Does the path escape above its starting point (has leading ".." after normalization)?
   ├─ YES → Use Fpath.t
   └─ NO  → Does it start from filesystem root?
            ├─ YES → Use Absolute_path.t
            └─ NO  → Use Relative_path.t
   ``` *)
