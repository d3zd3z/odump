open Ocamlbuild_plugin
open Command

let _ = dispatch begin function
  | Before_options ->
    (* Force ocamlfind.  Makes it easier to invoke the build without make rules. *)
    Options.use_ocamlfind := true;

  | After_rules ->
    (* dep ["link"; "ocaml"; "use_dbunix"] ["dbunix_stubs.o"]; *)

    (* Keep sources around. *)
    flag ["ocaml"; "compile"; "native"] (S[A "-S"]);

    dep ["link"; "ocaml"; "native"; "use_dbunix"] ["libdbunix.a"];
    dep ["link"; "ocaml"; "byte"; "use_dbunix"] ["dlldbunix.so"];
  | _ -> ()
end
