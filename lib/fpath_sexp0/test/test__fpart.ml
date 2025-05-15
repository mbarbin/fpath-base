(* We keep this file to test the [ocamlmig] migrate annotations regarding [Fpart]. *)

[@@@ocaml.alert "-deprecated"]

let _ = Fpart.v "hello"
