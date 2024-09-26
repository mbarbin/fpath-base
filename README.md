# fpath-base

[![CI Status](https://github.com/mbarbin/fpath-base/workflows/ci/badge.svg)](https://github.com/mbarbin/fpath-base/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/mbarbin/fpath-base/badge.svg?branch=main)](https://coveralls.io/github/mbarbin/fpath-base?branch=main)

This repository defines 2 OCaml packages named `fpath-sexplib0` and `fpath-base`, which extend the [fpath](https://opam.ocaml.org/packages/fpath/) package for projects using [sexplib0](https://opam.ocaml.org/packages/sexplib0/) and/or [base](https://opam.ocaml.org/packages/base/).

## fpath-sexplib0

This package only depends on `fpath` and `sexplib0`. It defines a single module, `Fpath_sexplib0`, which is designed to be opened to shadow the `Fpath` module. This introduces three new modules to the scope: `Fpart`, `Absolute_path` and `Relative_path`.

**Fpath** is shadowed and retains all its original functionality, with the addition of a sexp serializer:

```ocaml
type t = Fpath.t

include module type of struct include Fpath end

val sexp_of_t : t -> Sexp.t
```

Additionally, it includes new helpers for casting between the types offered by the package, such as:

```ocaml
val classify
  :  Fpath.t
  -> [ `Absolute of Absolute_path.t | `Relative of Relative_path.t ]
```

**Fpart** is a helper module for representing and manipulating the elements that constitute the '/' separated segments of a path.

**Absolute_path** and **Relative_path** are helper modules that distinguish between classes of paths in the type system, enhancing type safety for applications manipulating paths. Both types are defined as `private Fpath.t`, making it easy to cast and convert paths.

## fpath-base

This package further extends `fpath-sexplib0` and adds a dependency on `base`. It is designed to be compatible with `Base`-style containers such as `Map`, `Set`, `Hashtbl`, `Hash_set`.

This package defines a single module, `Fpath_base`, which is designed to be opened to shadow and further extend the four modules from `fpath-sexplib0`: `Fpath`, `Fpart`, `Absolute_path` and `Relative_path`. It exports `hashable` and `comparable` interfaces.

```ocaml
module Fpath : sig
  type t = Fpath.t

  include module type of Fpath_sexp0.Fpath with type t := t
  include Comparable.S with type t := t

  val hash : t -> int
  val hash_fold_t : Hash.state -> t -> Hash.state
end
```

This allows for example:

```ocaml file=example.ml
open! Base
open! Fpath_base

let create_fpath_table () = Hashtbl.create (module Fpath)
```

and in the mli respectively:

```ocaml file=example.mli
open! Base

val create_fpath_hashtbl : unit -> _ Hashtbl.M(Fpath).t
```

## Usage

- `Fpath_sexplib0` only adds the `sexplib0` dependency to `fpath`, making it suitable for various applications.
- `Fpath_base` is useful for applications that already depend on `Base`.

The intended usage for these packages is to open the single module they define and then continue using `Fpath` as usual.

You may do this either by opening the package in your `ml/mli` files or via `dune` flags, such as:

```dune
 (flags -open Fpath_sexplib0)
```

## Motivations

We liked the `Fpath` module and wanted to make it seamlessly compatible with projects using `sexp` and `base`.

## Code Documentation

The code documentation of the latest release is built with `odoc` and published to `GitHub` pages [here](https://mbarbin.github.io/fpath-base).

## Known Limitations

At the moment we only needed and tested the library on `ubuntu-latest`. We assume it is *broken* on Windows. See this [discussion](https://github.com/mbarbin/fpath-base/discussions/7).

## Acknowledgements

We would like to thank to Daniel Bünzli and the fpath programmers for the original `fpath` package, which this project extends. The implementation of `Absolute_path` and `Relative_path` is based on functionality from the `Fpath` module. We greatly appreciate Daniel's contribution to the open source community and the foundational work they provided, which has been instrumental in the development of this project. `Fpath`'s copyright and permission notice are included at the root of this project, in the file `LICENSE.fpath`.

We would also like to acknowledge the `Path` module from Jane Street's Iron code review system, which we took some inspiration from during the development of the module `Path` in this project. Specifically, we were inspired by their idea of distinguishing between absolute and relative paths at the type level. While we did not reuse any of the implementation from the original `Path` module, the general concepts and some function names are similar due to the nature of the file path operations we are performing (such as `extend` and `concat` for extending and concatenating paths, etc.). We appreciate the work done by the Jane Street team and their contribution to the open source community. Their project is licensed under the Apache License version 2.0, which can be found [here](https://github.com/janestreet/iron).
