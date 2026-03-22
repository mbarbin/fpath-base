# Migration Guide: Version 0.4.0

## Summary

Version 0.4.0 makes `Relative_path.t` reject paths that escape above their
starting point (paths with leading `..` segments after normalization).

## Breaking Changes

### Construction Functions

All construction functions (`v`, `of_string`, `of_fpath`) now reject
escaping paths:

```ocaml
Relative_path.v "../config" raises:
Invalid_argument "Relative_path.v: path \"../config\" escapes above starting point"
```

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

```ocaml
Relative_path.parent Relative_path.empty => None
```

This fixes infinite loops in upward navigation:

```ocaml
navigate_to_root (Relative_path.v "a/b/c") => "./"
```

### Extend Function

Raises `Invalid_argument` if extending creates an escaping path:

```ocaml
Relative_path.extend Relative_path.empty (Fsegment.v "..") raises:
Invalid_argument "Relative_path.extend: path \"./..\" escapes above starting point"
```

**Migration:** Use `Fpath.t` if segments might create escaping paths.

### Chop Prefix/Suffix

Empty prefix/suffix now returns `Some path` (previously `None`):

```ocaml
chop_prefix (Relative_path.v "foo/bar") ~prefix:empty => "foo/bar"
```

## Common Migration Patterns

### Dynamic Path Construction

Validate paths and handle rejections:

```ocaml
load_relative_file "config/settings.conf" => Ok "config/settings.conf"
```

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
details.
