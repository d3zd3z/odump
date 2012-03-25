(* Host side of remote commands.
   These are the operations done from the side that the user is running the command on. *)

module LegacyUnix = Unix

open Batteries_uni
open Remote

module Unix = LegacyUnix
module Marshal = Legacy.Marshal

let client = ref Config.bogus_client

type pair = { send: out_channel;
	      receive: in_channel }

module Unsafe = struct
  let send con msg =
    Marshal.to_channel con.send msg [];
    Legacy.flush con.send
  let receive con = Marshal.from_channel con.receive
end

let connect () =
  let (send_rd, send_wr) = Unix.pipe () in
  let (receive_rd, receive_wr) = Unix.pipe () in
  let (cmd, args) = (!client).Config.client_command in
  let cmd = Shell.cmd cmd args in
  (* TODO: How to communicate errors back? *)
  let _ = Thread.create (fun () ->
    Std.finally (fun () ->
      Unix.close send_rd;
      Unix.close receive_wr)
      (Shell.call ~stdin:(Shell.from_fd send_rd)
	  ~stdout:(Shell.to_fd receive_wr))
      [ cmd ]) () in
  let con = { send = Unix.out_channel_of_descr send_wr;
	      receive = Unix.in_channel_of_descr receive_rd } in
  Unsafe.send con version;
  let reply = (Unsafe.receive con : int) in
  if reply <> version then begin
    Unix.close send_rd;
    Unix.close receive_rd;
    Log.failf "Protocol mismatch with remote, ours=%d, theirs=%d" version reply
  end;
  con

let client_send con (msg : client_request) = Unsafe.send con msg
let client_receive con : [> client_reply ] = Unsafe.receive con

let ping () =
  let con = connect () in
  client_send con (`Ping "Hello world");
  match client_receive con with
    | `Pong msg -> Log.infof "Ping: %S" msg
    | _ -> Log.fail "Unknown reply"
