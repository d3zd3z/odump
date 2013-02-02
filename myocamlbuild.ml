open Ocamlbuild_plugin
open Command

(* Invoke "git describe" to determine the version of the tree. *)
let git_version =
  let chan = Unix.open_process_in "git describe --dirty --always" in
  let version = input_line chan in
  close_in chan;
  version

let make_version _ _ =
  let cmd = Printf.sprintf "let version = %S" git_version in
  Cmd (S [ A "echo"; Quote (Sh cmd); Sh ">"; P "version.ml" ])

let _ = dispatch begin function
  | Before_options ->
    (* Force ocamlfind.  Makes it easier to invoke the build without make rules. *)
    Options.use_ocamlfind := true;

  | After_rules ->
    (* dep ["link"; "ocaml"; "use_dbunix"] ["dbunix_stubs.o"]; *)

    (* Keep sources around. *)
    flag ["ocaml"; "compile"; "native"] (S[A "-S"]);
    flag ["c"; "compile"] (S[A "-ccopt"; A "-g"]);

    dep ["link"; "ocaml"; "native"; "use_dbunix"] ["libdbunix.a"];
    dep ["link"; "ocaml"; "byte"; "use_dbunix"] ["dlldbunix.so"];

    (* Generate version string. *)
    rule "version.ml" ~prod:"version.ml" make_version
  | _ -> ()
end
