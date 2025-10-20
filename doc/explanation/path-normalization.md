# Path Normalization and Escaping Prevention

## Overview

Starting in version 0.4.0, `Relative_path.t` rejects paths that escape above their starting point.

**The process:**
1. Paths are normalized using `Fpath.normalize` (resolves `.` and `..` segments)
2. If the normalized path has leading `..` segments, it's rejected by `Relative_path.t`

## What Gets Rejected

Paths that escape above their starting point:

```ocaml
# Relative_path.v ".." ;;
Exception:
Invalid_argument "Relative_path.v: path \"..\" escapes above starting point".
# Relative_path.v "../config" ;;
Exception:
Invalid_argument
 "Relative_path.v: path \"../config\" escapes above starting point".
# Relative_path.v "a/../.." ;;
Exception:
Invalid_argument
 "Relative_path.v: path \"a/../..\" escapes above starting point".
```

Paths that stay within bounds are accepted:

```ocaml
# Relative_path.to_string (Relative_path.v "a/..") ;;
- : string = "./"
# Relative_path.to_string (Relative_path.v "a/b/../c") ;;
- : string = "a/c"
```

## Why This Matters

### Prevents Memory Growth

Before v0.4.0, calling `parent` repeatedly could grow memory unboundedly:

<!-- $MDX skip -->
```ocaml
(* Before: Starting from "./" *)
parent "./"    (* -> "../" *)
parent "../"   (* -> "../../" *)
parent "../../" (* -> "../../../" ... forever *)
```

After v0.4.0:

```ocaml
# Relative_path.parent Relative_path.empty ;;
- : Relative_path.t option = None
```

### Type Safety Guarantee

`Relative_path.t` now guarantees the path won't escape above its starting point, making it safe for:
- Sandbox operations (can't escape sandbox root)
- Archive extraction (can't write outside target directory)
- Path concatenation (stays within base directory)

## When You Need Escaping Paths

If you need paths with leading `..` segments, use `Fpath.t` directly:

```ocaml
# let path : Fpath.t = Fpath.v "../config" |> Fpath.normalize ;;
val path : Fpath.t = <abstr>
```

## Type Selection Guide

```
Does the path escape above its starting point (has leading ".." after normalization)?
├─ YES → Use Fpath.t
└─ NO  → Does it start from filesystem root?
         ├─ YES → Use Absolute_path.t
         └─ NO  → Use Relative_path.t
```
