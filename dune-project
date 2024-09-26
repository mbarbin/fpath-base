(lang dune 3.16)

(name fpath-base)

(generate_opam_files)

(license MIT)

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/fpath-base))

(documentation "https://mbarbin.github.io/fpath-base/")

(package
 (name fpath-base)
 (synopsis "Adds a few functions to Fpath to use alongside Base")
 (depends
  (ocaml
   (>= 5.2))
  (base
   (and
    (>= v0.17)
    (< v0.18)))
  (fpath
   (>= 0.7.3))
  (fpath-sexp0
   (= :version))))

(package
 (name fpath-sexp0)
 (synopsis
  "Adds Fpath.sexp_of_t and defines 3 new modules: Fpart, Absolute_path and Relative_path")
 (depends
  (ocaml
   (>= 5.2))
  (fpath
   (>= 0.7.3))
  (sexplib0
   (and
    (>= v0.17)
    (< v0.18)))))

(package
 (name fpath-base-tests)
 (synopsis "Tests for fpath-base")
 (depends
  (ocaml
   (>= 5.2))
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.26.2)))
  (base
   (and
    (>= v0.17)
    (< v0.18)))
  (base_quickcheck
   (and
    (>= v0.17)
    (< v0.18)))
  (bisect_ppx
   (and
    :with-dev-setup
    (>= 2.8.3)))
  (expect_test_helpers_core
   (and
    (>= v0.17)
    (< v0.18)))
  (fpath
   (>= 0.7.3))
  (fpath-base
   (= :version))
  (fpath-sexp0
   (= :version))
  (ppx_compare
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_enumerate
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_expect
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_hash
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_here
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_js_style
   (and
    :with-dev-setup
    (>= v0.17)
    (< v0.18)))
  (ppx_let
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_conv
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_value
   (and
    (>= v0.17)
    (< v0.18)))
  (ppxlib
   (>= 0.33))
  (re
   (>= 1.8.0))
  (sexp_pretty
   (and
    (>= v0.17)
    (< v0.18)))
  (stdio
   (and
    (>= v0.17)
    (< v0.18)))))
