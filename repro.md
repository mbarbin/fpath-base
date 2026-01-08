At rev: 41f8fff63687de56a8693a8add4ad1af9e8060b5

```sh
$ dune-pkg build --workspace=dune-workspace.ci --display=short --only-packages=fpath-sexp0
...
File "dune.lock/ocaml-compiler.5.4.0.pkg", line 9, characters 8-15:
Error: Logs for package ocaml-compiler
/usr/bin/install: cannot create regular file '/home/mathieu/.cache/dune/toolchains/ocaml-compiler.5.4.0-913d1c0e16c16baef7f099ae5b9afd32/target/bin/ocamlc.byte': File exists
gmake: *** [Makefile:2670: install] Error 1
```
