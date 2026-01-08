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
