# fpath-base

[![CI Status](https://github.com/mbarbin/fpath-base/workflows/ci/badge.svg)](https://github.com/mbarbin/fpath-base/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/mbarbin/fpath-base/badge.svg?branch=main)](https://coveralls.io/github/mbarbin/fpath-base?branch=main)

This library extends the functionality offered by the
[fpath](https://opam.ocaml.org/packages/fpath/) package to be used by projects
using [base](https://opam.ocaml.org/packages/base/).

Specifically, it adds the following `ppx` to the main `Fpath.t` type as follows:

```ocaml
type t = Fpath.t [@@deriving compare, equal, hash, sexp_of]

include Comparable.S with type t := t
```

thus allowing for example:

```ocaml file=example.ml
open! Base
open! Fpath_base

let create_fpath_table () = Hashtbl.create (module Fpath)
```

and in the mli

```ocaml file=example.mli
open! Base

val create_fpath_hashtbl : unit -> _ Hashtbl.M(Fpath).t
```

The package also defines modules to distinguish between absolute and relative
path at the type level (`Absolute_path.t` & `Relative_path.t`).

## Usage

The intended usage is to open the module `Fpath_base`, and keep on simply
using `Fpath` as usual.

## Motivations

This package started as a personal experiment. Perhaps it will grow into
something that I would consider submitting to the opam repository. TBD.

## Code documentation

The code documentation of the latest release is built with `odoc` and published
to `GitHub` pages [here](https://mbarbin.github.io/fpath-base).

## Acknowledgements

We would like to thank to Daniel Bünzli and the fpath programmers for the
original `fpath` package, which this project extends. The implementation of
`Absolute_path` and `Relative_path` is based on functionality from the `Fpath`
module. We greatly appreciate Daniel's contribution to the open source community
and the foundational work they provided, which has been instrumental in the
development of this project. `Fpath`'s copyright and permission notice are
included at the root of this project, in the file `LICENSE.fpath`.

We would also like to acknowledge the `Path` module from Jane Street's Iron code
review system, which we took some inspiration from during the development of the
module `Path` in this project. Specifically, we were inspired by their idea of
distinguishing between absolute and relative paths at the type level. While we
did not reuse any of the implementation from the original `Path` module, the
general concepts and some function names are similar due to the nature of the
file path operations we are performing (such as `extend` and `concat` for
extending and concatenating paths, etc.). We appreciate the work done by the
Jane Street team and their contribution to the open source community. Their
project is licensed under the Apache License version 2.0, which can be found
[here](https://github.com/janestreet/iron).
