(* The remote receiver *)

open Batteries_uni
open Remote

module Marshal = Legacy.Marshal

module Unsafe = struct
  (* Fully polymorphic send routines. *)
  let receive () =
    Marshal.from_channel Legacy.stdin

  let send msg =
    Marshal.to_channel Legacy.stdout msg [];
    Legacy.flush Legacy.stdout
end

let client_receive () : [> client_request ] = Unsafe.receive ()
let client_send (msg : client_reply) = Unsafe.send msg

let process () =
  let host_version = (Unsafe.receive () : int) in
  if host_version <> version then begin
    Unsafe.send (-version);
    Log.failure ("Protocol mismatch with remote", ["receiver", string_of_int version;
						   "host", string_of_int host_version])
  end;
  Unsafe.send version;
  match client_receive () with
    | `Ping msg ->
      client_send (`Pong ("received: " ^ msg))
    | _ ->
      Printf.eprintf "Invalid message\n%!"
