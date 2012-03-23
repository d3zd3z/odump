(* The remote protocol itself. *)

let version = 1
(** The version of the data in this entire file.  This should be
    changed any time this protocol changes.  Adding new types should
    be safe, since the receiver will just reject unknown top-level
    request.  Changing the types within any request, though, should
    result in a version change.  The version number should be a
    positive integer.  Negative values will be used to indicate
    version mismatches.  *)

(* The client command protocol.  Requests are sent to the remote
   client, and the client replies with the reply.  Some of the
   commands will switch roles, but only after the server receives the
   reply. *)

type client_request =
  [ `Ping of string ]

type client_reply =
  [ `Pong of string ]
