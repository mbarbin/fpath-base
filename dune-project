(lang dune 3.17)

(name fpath-base)

(generate_opam_files)

(license MIT)

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/fpath-base))

(documentation "https://mbarbin.github.io/fpath-base/")

(using mdx 0.4)

;; The value for the [implicit_transitive_deps] option is set during the CI
;; depending on the OCaml compiler version.
;;
;; This will be set to [false] iif [ocaml-version >= 5.2].
;;
;; For packaging purposes with older ocaml, it is simpler atm if the option is
;; set to [true] in the main branch.
;;
;; See: [.github/workflows/edit_dune_project_dot_ml].

(implicit_transitive_deps true)

(package
 (name fpath-base)
 (synopsis "Adds a few functions to Fpath to use alongside Base")
 (depends
  (ocaml
   (>= 5.2))
  (base
   (>= v0.17))
  (fpath
   (>= 0.7.3))
  (fpath-sexp0
   (= :version))))

(package
 (name fpath-sexp0)
 (synopsis
  "Adds Fpath.sexp_of_t and defines 3 new modules: Fsegment, Absolute_path and Relative_path")
 (depends
  (ocaml
   (>= 4.14))
  (fpath
   (>= 0.7.3))
  (sexplib0
   (>= v0.16))))

(package
 (name fpath-base-tests)
 (synopsis "Tests for fpath-base")
 (depends
  (ocaml
   (>= 5.2))
  (base
   (>= v0.17))
  (dyn
   (>= 3.17))
  (expect_test_helpers_core
   (>= v0.17))
  (fpath
   (>= 0.7.3))
  (fpath-base
   (= :version))
  (fpath-sexp0
   (= :version))
  (pp
   (>= 2.0.0))
  (ppx_compare
   (>= v0.17))
  (ppx_expect
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.33))))

(package
 (name fpath-base-dev)
 (synopsis "Package to regroup dev targets, documentation, and more")
 (allow_empty)
 (depends
  (ocaml
   (>= 5.2))
  (ocamlformat
   (= 0.28.1))
  (base
   (>= v0.17))
  (bisect_ppx
   (>= 2.8.3))
  (expect_test_helpers_core
   (>= v0.17))
  (fpath
   (>= 0.7.3))
  (fpath-base
   (= :version))
  (fpath-base-tests
   (= :version))
  (fpath-sexp0
   (= :version))
  (mdx
   (>= 2.4))
  (ppx_compare
   (>= v0.17))
  (ppx_expect
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_js_style
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (sherlodoc
   (and
    :with-doc
    (>= 0.2)))))
