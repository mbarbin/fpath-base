# Dune version

`dune-pkg` is a nightly built dune available in my PATH:

```sh
$ dune-pkg --version
"Nightly build 2026-01-08T02:56:04Z, git revision
ddefc9902ce3fbc7dcfadda5c2c8594d236868c0"
```

# Repro 1

At rev: 41f8fff63687de56a8693a8add4ad1af9e8060b5

```sh
$ dune-pkg build --workspace=dune-workspace.ci --display=short --only-packages=fpath-sexp0
...
File "dune.lock/ocaml-compiler.5.4.0.pkg", line 9, characters 8-15:
Error: Logs for package ocaml-compiler
/usr/bin/install: cannot create regular file '/home/mathieu/.cache/dune/toolchains/ocaml-compiler.5.4.0-913d1c0e16c16baef7f099ae5b9afd32/target/bin/ocamlc.byte': File exists
gmake: *** [Makefile:2670: install] Error 1
```

----

# Repro 2

At rev: 5e322a3a65d98f030856f5e41d581992d2e2406b

```sh
$ dune-pkg build --workspace=dune-workspace.ci --display=short --only-packages=fpath-sexp0
Internal error, please report upstream including the contents of _build/log.
Description:
  ("[gen_rules] did not specify rules for the context",
   { context_name = "_private" })
Raised at Stdune__Code_error.raise in file "stdune__Code_error.ml", line 11,
  characters 30-62
Called from Fiber__Scheduler.exec in file "fiber__Scheduler.ml", line 77,
  characters 8-11
-> required by ("gen-rules", In_build_dir "_private/default")
-> required by ("gen-rules", In_build_dir "_private/default/.lock")
-> required by ("load-dir", In_build_dir "_private/default/.lock")
-> required by
   ("build-file", In_build_dir "_private/default/.lock/dune-4.14.lock")
-> required by ("<unnamed>", ())
-> required by ("<unnamed>", ())
-> required by ("gen-rules", In_build_dir "default-4.14")
-> required by ("load-dir", In_build_dir "default-4.14")
-> required by
   ("build-alias", { dir = In_build_dir "default-4.14"; name = "default" })
-> required by ("toplevel", ())

I must not crash.  Uncertainty is the mind-killer. Exceptions are the
little-death that brings total obliteration.  I will fully express my cases. 
Execution will pass over me and through me.  And when it has gone past, I
will unwind the stack along its path.  Where the cases are handled there will
be nothing.  Only I will remain.
```

# Repro 3

At rev: 629b7a0e3ce67120b4c9edd827e3017649995abf

```sh
$ dune-pkg build --workspace=dune-workspace.ci --display=short --only-packages=fpath-sexp0,fpath-base
File "default/.lock/_unknown_", line 1, characters 0-0:
Error: Couldn't solve the package dependency formula.
Selected candidates: astring.0.8.5 base.v0.17.3 base-unix.base csexp.1.5.2
                     dune-configurator.3.20.2 fpath.0.7.3 fpath-base.dev
                     fpath-sexp0.dev ocaml_intrinsics_kernel.v0.17.1
                     ocamlbuild.0.16.1+dune ocamlfind.1.9.8+dune
                     sexplib0.v0.17.0 topkg.1.1.1 fpath-base&fpath-sexp0
- dune -> dune.3.22
    User requested = 3.22
- ocaml -> (problem)
    No usable implementations:
      ocaml.5.5.0:
        Package does not satisfy constraints of local package fpath-sexp0
      ocaml.5.4.1:
        Package does not satisfy constraints of local package fpath-sexp0
      ocaml.5.4.0:
        Package does not satisfy constraints of local package fpath-sexp0
      ocaml.5.3.1:
        Package does not satisfy constraints of local package fpath-sexp0
      ocaml.5.3.0:
        Package does not satisfy constraints of local package fpath-sexp0
      ...
```

The error is confusing, I would expect it to explain that `fpath-base` is not
available in the context `4.14` which is built by default.

# Question?

If `dune build` builds all context available, but not all packages are available
on all contextes (like this is the case here), question is: how do you organize
your CI?
