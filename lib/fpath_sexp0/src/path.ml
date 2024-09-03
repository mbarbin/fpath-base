type absolute_path = Fpath.t
type relative_path = Fpath.t

let append a r = Fpath.(a // r) |> Fpath.normalize
let extend t f = Fpath.(t / File_name.to_string f) |> Fpath.normalize

let parent t =
  let t' = Fpath.normalize t |> Fpath.parent in
  if Fpath.equal t t' then None else Some t'
;;

let chop_prefix t ~prefix =
  match Fpath.rem_prefix prefix t with
  | Some t -> Some t
  | None -> if Fpath.equal prefix t then Some Fpath.(v "./") else None
;;

let chop_suffix ~empty t ~suffix =
  let rec aux t suffix =
    match t, suffix with
    | _, [] -> Some t
    | [], _ :: _ -> None
    | hd :: t, hd2 :: suffix -> if String.equal hd hd2 then aux t suffix else None
  in
  match aux (Fpath.segs t |> List.rev) (Fpath.segs suffix |> List.rev) with
  | Some ([] | [ "" ]) -> Some empty
  | Some (_ :: _ as segs) ->
    let result = String.concat Fpath.dir_sep (List.rev segs) in
    Some (Fpath.v result)
  | None -> None
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
         Error (`Msg (Printf.sprintf "%S: not an absolute path" (f |> Fpath.to_string))))
  ;;

  let v str =
    match str |> of_string with
    | Ok t -> t
    | Error (`Msg m) -> invalid_arg m
  ;;

  let root = Fpath.v Fpath.dir_sep
  let append = append
  let extend = extend
  let parent = parent
  let chop_prefix t ~prefix = chop_prefix t ~prefix
  let chop_suffix t ~suffix = chop_suffix ~empty:root t ~suffix
  let is_dir_path = Fpath.is_dir_path
  let to_dir_path = Fpath.to_dir_path

  let relativize ~root f =
    let f = Fpath.normalize f in
    if Fpath.is_abs f then f else append root f
  ;;
end

module Relative_path = struct
  include Fpath0

  let to_fpath t = t
  let to_string = Fpath.to_string

  let of_fpath f =
    let f = Fpath.normalize f in
    if Fpath.is_rel f then Some f else None
  ;;

  let of_string str =
    match Fpath.of_string str with
    | Error (`Msg _) as error -> error
    | Ok f ->
      (match of_fpath f with
       | None ->
         Error (`Msg (Printf.sprintf "%S: not a relative path" (f |> Fpath.to_string)))
       | Some t -> Ok t)
  ;;

  let v str =
    match str |> of_string with
    | Ok t -> t
    | Error (`Msg m) -> invalid_arg m
  ;;

  let empty = Fpath.v "./"
  let append = append
  let extend = extend
  let parent = parent
  let of_list files = List.fold_left extend empty files
  let chop_prefix t ~prefix = chop_prefix t ~prefix
  let chop_suffix t ~suffix = chop_suffix ~empty t ~suffix
  let is_dir_path = Fpath.is_dir_path
  let to_dir_path = Fpath.to_dir_path
end

module Export = struct
  let classify f =
    let f = Fpath.normalize f in
    if Fpath.is_abs f then `Absolute f else `Relative f
  ;;
end
