## 0.0.11 (unreleased)

### Added

### Changed

- Remove vendor, use `expect_test_helpers_core.expect_test_helpers_base`.

### Deprecated

### Fixed

### Removed

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
