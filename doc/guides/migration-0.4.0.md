# Migration Guide: Version 0.4.0

## Summary

Version 0.4.0 makes `Relative_path.t` reject paths that escape above their starting point (paths with leading `..` segments after normalization).

## Breaking Changes

### Construction Functions

All construction functions (`v`, `of_string`, `of_fpath`) now reject escaping paths:

```ocaml
# Relative_path.v "../config" ;;
Exception:
Invalid_argument
 "Relative_path.v: path \"../config\" escapes above starting point".
```

**Migration options:**

Use `Absolute_path.t` for explicit paths:
```ocaml
# let path = Absolute_path.v "/path/to/parent/config" ;;
val path : Absolute_path.t = <abstr>
```

Or use `Fpath.t` for paths that may escape:
```ocaml
# let path : Fpath.t = Fpath.v "../config" |> Fpath.normalize ;;
val path : Fpath.t = <abstr>
```

### Parent Function

Returns `None` for the empty path (previously returned `"../"`):

```ocaml
# Relative_path.parent Relative_path.empty ;;
- : Relative_path.t option = None
```

This fixes infinite loops in upward navigation:

```ocaml
# let rec navigate_to_root path =
    match Relative_path.parent path with
    | None -> path
    | Some p -> navigate_to_root p ;;
val navigate_to_root : Relative_path.t -> Relative_path.t = <fun>
# Relative_path.to_string (navigate_to_root (Relative_path.v "a/b/c")) ;;
- : string = "./"
```

### Extend Function

Raises `Invalid_argument` if extending creates an escaping path:

```ocaml
# Relative_path.extend Relative_path.empty (Fsegment.v "..") ;;
Exception:
Invalid_argument
 "Relative_path.extend: path \"./..\" escapes above starting point".
```

**Migration:** Use `Fpath.t` if segments might create escaping paths.

### Chop Prefix/Suffix

Empty prefix/suffix now returns `Some path` (previously `None`):

```ocaml
# match Relative_path.chop_prefix (Relative_path.v "foo/bar") ~prefix:Relative_path.empty with
  | None -> "no match"
  | Some p -> Relative_path.to_string p ;;
- : string = "foo/bar"
```

## Common Migration Patterns

### Dynamic Path Construction

Validate paths and handle rejections:

```ocaml
# let load_relative_file filename =
    match Relative_path.of_string filename with
    | Error (`Msg err) -> Error err
    | Ok path -> Ok path ;;
val load_relative_file : string -> (Relative_path.t, string) result = <fun>
# load_relative_file "config/settings.conf" ;;
- : (Relative_path.t, string) result = Ok <abstr>
```

### Upward Navigation

Use absolute paths for upward traversal:

```ocaml
# let find_project_root has_marker current_path =
    let rec search path =
      if has_marker path then Some path
      else
        match Absolute_path.parent path with
        | None -> None
        | Some parent -> search parent
    in
    search current_path ;;
val find_project_root :
  (Absolute_path.t -> bool) -> Absolute_path.t -> Absolute_path.t option =
  <fun>
```

## Why These Changes

1. **Improve type safety** - `Relative_path.t` guarantees non-escaping
2. **Less error-prone APIs** for sandbox operations and recursive parent traversal

See [Path Normalization](../explanation/path-normalization.md) for more details.
