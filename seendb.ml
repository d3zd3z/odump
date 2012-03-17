(* Database of files that have been seen. *)

open Batteries_uni

let schema = {
  (* The schema version covers both the SQL schema, and the structure
     of the 'dir_node' list. *)
  Db.schema_version = "1:2012-03-16";
  Db.schema_text =
    [| "create table seen (pino integer not null, expire double not null, \
info blob not null)";
       "create index seen_index on seen(pino)";
       "create index seen_expire_index on seen(expire)" |]
}

module SM = Map.StringMap
module I64Map = Map.Make(Int64)

type dir_node = {
  n_inode : int64;
  n_ctime : float;
  n_expire : float;
  n_hash : Hash.t
}

type dir_info = {
  pino : int64;
  min_expire : float;
  nodes : dir_node list
}

let get_int64 key map = Int64.of_string (SM.find key map)
let get_time key map = Dbunix.float_of_time (SM.find key map)
let get_hash key map = Hash.of_string (SM.find key map)

let start_time = Unix.gettimeofday ()
let randomize_expire () =
  start_time +. Random.float (28.0 *. 86400.0) +. (14.0 *. 86400.0)

class seen_update_visitor db =
object
  inherit Nodes.empty_visitor
  method! want_full_data = false

  val dirs = (Stack.create () : dir_node Stack.t Stack.t)

  method! enter path chunk node = match node with
    | Nodes.NodeNode ("DIR", _) -> Stack.push (Stack.create ()) dirs
    | Nodes.NodeNode ("REG", _) -> raise Nodes.Prune
    | _ -> ()

  initializer Db.sql0 db "begin transaction" []

  method! leave path chunk node = match node with
    | Nodes.NodeNode ("DIR", props) ->
      let nodes = Stack.pop dirs in
      let pino = get_int64 "ino" props in
      let nodes = List.of_enum (Stack.enum nodes) in
      if nodes = [] then
	Db.sql0 db "delete from seen where pino = ?" [Db.Data.INT pino]
      else begin
	let min_expire = List.fold_left (fun a n -> min a n.n_expire) max_float nodes in
	Db.sql0 db "insert into seen values (?, ?, ?)"
	  [ Db.Data.INT pino;
	    Db.Data.FLOAT min_expire;
	    Db.Data.BLOB (Marshal.to_string nodes []) ]
      end
    (* Log.with_output (fun () -> *)
    (* 	Pdump.pdump (Marshal.to_string nodes [])); *)

    | Nodes.NodeNode ("REG", props) ->
      let top = Stack.top dirs in
      let ctime = get_time "ctime" props in
      let node = { n_inode = get_int64 "ino" props;
		   n_ctime = ctime;
		   n_expire = randomize_expire ();
		   n_hash = get_hash "data" props } in
      Stack.push node top
    | _ -> ()

  method commit = Db.sql0 db "commit" []
end

let make_cache pool node =
  let db = Db.connect "/tmp/blah.sqlite" schema in
  let visit = new seen_update_visitor db in
  Nodes.walk pool "." node (visit :> Nodes.visitor);
  visit#commit
