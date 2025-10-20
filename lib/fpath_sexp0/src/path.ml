(*********************************************************************************)
(*  fpath-base: Extending [Fpath] to use alongside [Sexplib0] and/or [Base]      *)
(*  SPDX-FileCopyrightText: 2023-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT                                                 *)
(*********************************************************************************)

type absolute_path = Fpath.t
type relative_path = Fpath.t

let is_parent_segment s = String.equal ".." s
let has_parent_segment segs = List.exists is_parent_segment segs

let normalize f =
  let t = Fpath.normalize f in
  if has_parent_segment (Fpath.segs t)
  then Error (Printf.sprintf "path %S escapes above starting point" (Fpath.to_string f))
  else Ok t
;;

let normalize_exn ~fn_name f =
  match normalize f with
  | Ok t -> t
  | Error msg -> invalid_arg (Printf.sprintf "%s: %s" fn_name msg)
;;

let empty_rel_path = Fpath.v ("." ^ Fpath.dir_sep)

let chop_suffix ~empty t ~suffix =
  if Fpath.equal suffix empty_rel_path
  then Some t
  else (
    let rec aux t suffix =
      match t, suffix with
      | _, [] -> Some t
      | [], _ :: _ -> None
      | hd :: t, hd2 :: suffix -> if String.equal hd hd2 then aux t suffix else None
    in
    match aux (Fpath.segs t |> List.rev) (Fpath.segs suffix |> List.rev) with
    | None -> None
    | Some ([] | [ "" ]) -> Some empty
    | Some (_ :: _ as segs) ->
      let result = String.concat Fpath.dir_sep (List.rev segs) in
      Some (Fpath.v result |> Fpath.to_dir_path))
;;

module Absolute_path = struct
  include Fpath0

  let to_fpath t = t
  let to_string = Fpath.to_string

  let of_fpath f =
    let f = Fpath.normalize f in
    if Fpath.is_abs f then Some f else None
  ;;

  let of_string str =
    match Fpath.of_string str with
    | Error (`Msg _) as error -> error
    | Ok f ->
      (match of_fpath f with
       | Some t -> Ok t
       | None ->
         Error (`Msg (Printf.sprintf "%S is not an absolute path" (f |> Fpath.to_string))))
  ;;

  let v str =
    match str |> of_string with
    | Ok t -> t
    | Error (`Msg m) -> invalid_arg ("Absolute_path.v: " ^ m)
  ;;

  let root = Fpath.v Fpath.dir_sep
  let append a r = Fpath.(a // r) |> Fpath.normalize
  let extend t f = Fpath.(t / Fsegment.to_string f) |> Fpath.normalize

  let parent t =
    let t' = Fpath.normalize t |> Fpath.parent in
    if Fpath.equal t t' then None else Some t'
  ;;

  let chop_prefix t ~prefix =
    match Fpath.rem_prefix prefix t with
    | Some t -> Some t
    | None -> if Fpath.equal prefix t then Some empty_rel_path else None
  ;;

  let chop_suffix t ~suffix = chop_suffix ~empty:root t ~suffix
  let is_dir_path = Fpath.is_dir_path
  let to_dir_path = Fpath.to_dir_path
  let rem_empty_seg = Fpath.rem_empty_seg

  let relativize ~root f =
    Fpath.normalize (if Fpath.is_abs f then f else Fpath.(root // f))
  ;;
end

module Relative_path = struct
  include Fpath0

  let to_fpath t = t
  let to_string = Fpath.to_string

  let of_fpath f =
    if Fpath.is_rel f
    then Some (normalize_exn ~fn_name:"Relative_path.of_fpath" f)
    else None
  ;;

  let of_string str =
    match Fpath.of_string str with
    | Error (`Msg _) as error -> error
    | Ok f ->
      if Fpath.is_rel f
      then (
        match normalize f with
        | Ok _ as ok -> ok
        | Error msg -> Error (`Msg msg))
      else
        Error (`Msg (Printf.sprintf "%S is not a relative path" (f |> Fpath.to_string)))
  ;;

  let v str =
    match str |> of_string with
    | Ok t -> t
    | Error (`Msg m) -> invalid_arg ("Relative_path.v: " ^ m)
  ;;

  let empty = empty_rel_path
  let append a r = Fpath.(a // r) |> normalize_exn ~fn_name:"Relative_path.append"

  let extend t f =
    Fpath.(t / Fsegment.to_string f) |> normalize_exn ~fn_name:"Relative_path.extend"
  ;;

  let parent t =
    if Fpath.equal t empty_rel_path
    then None
    else Some (normalize_exn ~fn_name:"Relative_path.parent" (Fpath.parent t))
  ;;

  let of_list files = List.fold_left extend empty files

  let chop_prefix t ~prefix =
    if Fpath.equal prefix empty_rel_path
    then Some t
    else (
      match Fpath.rem_prefix prefix t with
      | Some t -> Some t
      | None -> if Fpath.equal prefix t then Some empty_rel_path else None)
  ;;

  let chop_suffix t ~suffix = chop_suffix ~empty t ~suffix
  let is_dir_path = Fpath.is_dir_path
  let to_dir_path = Fpath.to_dir_path
  let rem_empty_seg = Fpath.rem_empty_seg
end

module Export = struct
  let classify f =
    if Fpath.is_abs f
    then `Absolute (Fpath.normalize f)
    else `Relative (normalize_exn ~fn_name:"Fpath.classify" f)
  ;;
end
