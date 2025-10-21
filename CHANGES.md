## 0.X.X (unreleased)

### Removed

- Remove deprecated `Fpart` module (use `Fsegment` instead, deprecated since 0.3.0) (#PR, @mbarbin).

## 0.4.0 (2025-10-20)

### Added

- Document handling of escaping relative paths (#20, @mbarbin).

### Changed

- `Relative_path.t` now rejects paths that escape above their starting point (#20, @mbarbin).

## 0.3.1 (2025-05-26)

### Changed

- Conditional set implicit transitive deps in CI depending on the compiler version (#16, @mbarbin).

## 0.3.0 (2025-05-15)

### Added

- Add ocamlmig annotations for deprecated parts (#15, @mbarbin).
- Add support for OCaml-4.14 to `fpath-sexp0` (#14, @mbarbin).

### Deprecated

- Deprecate `Fpart` - renamed `Fsegment` and available since `0.2.2`.

## 0.2.2 (2024-10-15)

### Added

- Added `rem_empty_seg` to `Absolute_path` and `Relative_path`.
- In `fpath-sexp0` export hash keys interfaces compatible with stdlib hash functors.

### Changed

- Rename `Fpart` to `Fsegment` to fit `Fpath` terminology.

### Fixed

- Fix occurrence of hard coded dir separator.

## 0.2.1 (2024-09-22)

### Changed

- Remove need for ppx preprocessing in `fpath-base`.

## 0.2.0 (2024-09-04)

### Changed

- Rename `File_name` into `Fpart`.
- Remove need for ppx preprocessing in `fpath-sexp0`.

## 0.1.0 (2024-09-03)

### Added

- Added minimal library depending on `sexplib0` only.

### Changed

- Rewrite `Fpath_base` on top of `Fpath_sexp0`.
- Remove vendor, use `expect_test_helpers_core.expect_test_helpers_base`.

## 0.0.10 (2024-07-26)

### Added

- Added dependabot config for automatically upgrading action files.

### Changed

- Upgrade `ppxlib` to `0.33` - activate unused items warnings.
- Upgrade `ocaml` to `5.2`.
- Upgrade `dune` to `3.16`.
- Upgrade base & co to `0.17`.

## 0.0.9 (2024-03-13)

### Changed

- Vendor `expect-test-helpers`.
- Rename project `fpath-base`.

## 0.0.8 (2024-03-05)

### Changed

- Uses `expect-test-helpers` (reduce core dependencies)
- Run `ppx_js_style` as a linter & make it a `dev` dependency.
- Upgrade GitHub workflows `actions/checkout` to v4.
- In CI, specify build target `@all`, and add `@lint`.
- List ppxs instead of `ppx_jane`.

## 0.0.7 (2024-02-22)

### Added

- Add `Fpath.classify` to dispatch between absolute and relative paths.

### Changed

- Clarify use of empty relative paths.
- Make behavior closer to `Fpath`'s "directoryness" when able.

### Removed

- Removed `Classified_path`.
- Removed `Relative_path.{dot,dot_slash}`. Replaced by `Relative_path.empty`.

## 0.0.6 (2024-02-21)

### Added

- Add new modules `Absolute_path`, `Relative_path` to distinguish absolute
  from relative path at the type level.
- Add new tests.

### Changed

- Design the package now so that it is meant to be open.
- Override the `Fpath` in the scope, so you can use e.g. `Fpath.sexp_of_t` directly.

### Removed

- Remove `Arg_type` and dependency into `core.command`. To be revisited.

## 0.0.5 (2024-02-14)

### Added

- Add new test && increase coverage.

### Changed

- Upgrade dune to `3.14`.
- Build the doc with sherlodoc available to enable the doc search bar.
- Couple `hash` with `hash_fold_t` for consistency.

## 0.0.4 (2024-02-09)

### Added

- Setup `bisect_ppx` for test coverage.

### Changed

- Internal changes related to the release process.
- Upgrade dune and internal dependencies.

## 0.0.3 (2024-01-18)

### Changed

- Internal changes related to build and release process.

## 0.0.2 (2023-11-12)

### Added

- Add `arg_type` to parse paths from params of core commands.

## 0.0.1 (2023-11-07)

Initial release.

### Added

- Add `hash` and `sexp_of`. This makes `Fpath_extended` compatible with Base
  Containers such as Map, Set, Hashtbl, etc.
