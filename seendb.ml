(* Database of files that have been seen. *)

open Batteries_uni

let schema = {
  (* The schema version covers both the SQL schema, and the structure
     of the 'dir_node' list. *)
  Db.schema_version = "1:2012-03-18";
  Db.schema_text =
    [| "create table seen (pino integer primary key, expire double not null, \
info blob not null)";
       "create index seen_index on seen(pino)";
       "create index seen_expire_index on seen(expire)" |]
}

module SM = Map.StringMap
module Int64Map = Map.Make(Int64)

type dir_node = {
  n_inode : int64;
  n_ctime : float;
  n_expire : float;
  n_hash : Hash.t
}

let get_int64 key map = Int64.of_string (SM.find key map)
let get_time key map = Dbunix.float_of_time (SM.find key map)
let get_hash key map = Hash.of_string (SM.find key map)

let start_time = Unix.gettimeofday ()
let randomize_expire () =
  start_time +. Random.float (28.0 *. 86400.0) +. (14.0 *. 86400.0)

let entry_of_node hash props =
  { n_inode = get_int64 "ino" props;
    n_ctime = get_time "ctime" props;
    n_expire = randomize_expire ();
    n_hash = hash }

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
	let min_expire = List.fold_left (fun a n -> max a n.n_expire) min_float nodes in
	Db.sql0 db "insert or replace into seen values (?, ?, ?)"
	  [ Db.Data.INT pino;
	    Db.Data.FLOAT min_expire;
	    Db.Data.BLOB (Marshal.to_string nodes []) ]
      end
    (* Log.with_output (fun () -> *)
    (* 	Pdump.pdump (Marshal.to_string nodes [])); *)

    | Nodes.NodeNode ("REG", props) ->
      let top = Stack.top dirs in
      let node = entry_of_node chunk#hash props in
      Stack.push node top
    | _ -> ()

  method commit = Db.sql0 db "commit" []
end

let make_cache pool cache_name node =
  let db = Db.connect cache_name schema in
  let visit = new seen_update_visitor db in
  Nodes.walk pool "." node (visit :> Nodes.visitor);
  visit#commit

(** {6 Update within backup} *)

type t = Db.t
type cache_entry = dir_node Int64Map.t

let open_cache path = Db.connect path schema

let close cache = Db.close cache

let with_cache path f = with_dispose ~dispose:close f (open_cache path)

let remove_expired nodes =
  Int64Map.filter (fun { n_expire } -> n_expire > start_time) nodes

let get cache pino =
  match Db.sqln cache "select info from seen where pino = ?" [Db.Data.INT pino] with
    | [ ] -> Int64Map.empty
    | [ [| Db.Data.BLOB data |] ] ->
      let nodes = (Marshal.from_string data 0 : dir_node list) in
      let as_pair ({ n_inode } as node) = (n_inode, node) in
      let nodes = Int64Map.of_enum (Enum.map as_pair (List.enum nodes)) in
      remove_expired nodes
    | [ items ] ->
      let items = Array.to_list items in
      let items = String.join ", " (List.map Db.Data.to_string_debug items) in
      Log.failf "Unexpected query response: %Ld [| %S |]" pino items
    | _ -> Log.failf "Not expecting multiple rows, for %Ld" pino

let update db pino entry =
  if Int64Map.is_empty entry then
    Db.sql0 db "delete from seen where pino = ?" [Db.Data.INT pino]
  else begin
    let nodes = Int64Map.enum entry in
    let nodes = Enum.map Tuple.Tuple2.second nodes in
    let nodes = List.of_enum nodes in
    let min_expire = List.fold_left (fun a n -> max a n.n_expire) min_float nodes in
    Db.sql0 db "insert or replace into seen values (?, ?, ?)"
      [ Db.Data.INT pino;
	Db.Data.FLOAT min_expire;
	Db.Data.BLOB (Marshal.to_string nodes []) ]
  end

let begin_transaction db = Db.sql0 db "begin transaction" []
let commit db = Db.sql0 db "commit" []
