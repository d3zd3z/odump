(* Store pool based around an SQL database. *)

(* open Batteries *)
(* open LegacyIO *)
(* open Printf *)

let schema = {
  Db.schema_version = "1:2014-03-13";
  Db.schema_text =
    [| "CREATE TABLE blobs ( \
          id INTEGER PRIMARY KEY, \
          oid BLOB UNIQUE NOT NULL, \
          kind TEXT, \
          size INTEGER, \
          zsize INTEGER, \
          data BLOB)";
        "CREATE INDEX blobs_oid ON blobs(oid)";
        "CREATE INDEX blobs_backs ON blobs(kind) WHERE kind = 'back'";
        "CREATE TABLE props ( \
          key TEXT PRIMARY KEY, \
          value TEXT)" |]
}

let create_sql_pool path =
  Unix.mkdir path 0o755;
  let blob_name = path ^ "/blobs" in
  Unix.mkdir blob_name 0o755;
  let db = Db.connect (path ^ "/data.db") schema in
  Db.close db

let sql_to_hash = function
  | [| Db.Data.BLOB bytes |] -> Hash.of_raw bytes
  | _ -> Log.failf "Invalid sqlite3 response type"

exception TODO of string
let open_sql_pool path =
  let blob_path = path ^ "/blobs" in
  let db_path = path ^ "/data.db" in
  Misc.ensure_directory ~what:"pool dir" path;
  Misc.ensure_file ~what:"pool dir" db_path;
  Misc.ensure_directory ~what:"pool.blobs" blob_path;
  let db = Db.connect db_path schema in

  let chunk_path oid =
    let text_oid = Hash.to_string oid in
    let dirname = String.sub text_oid 0 2 in
    let filename = String.sub text_oid 2 (String.length text_oid - 2) in
    let dname = blob_path ^ "/" ^ dirname in
    (dname, dname ^ "/" ^ filename) in

  let write_chunk oid data =
    let dname, fname = chunk_path oid in
    let wfd =
      try
        open_out_gen [Open_wronly; Open_creat; Open_binary] 0o644 fname
      with
        | Sys_error _ ->
            Unix.mkdir dname 0o755;
            open_out_gen [Open_wronly; Open_creat; Open_binary] 0o644 fname
    in
    output_string wfd data;
    close_out wfd in

  let read_chunk oid zsize =
    let buf = String.create zsize in
    let _, fname = chunk_path oid in
    let rfd = open_in_bin fname in
    (* TODO: Close on error *)
    really_input rfd buf 0 zsize;
    buf in

  let in_transaction = ref false in

  let begin_transaction () =
    if not !in_transaction then begin
      Db.sql0 db "begin transaction" [];
      in_transaction := true
    end in

  let commit () =
    if !in_transaction then begin
      Db.sql0 db "commit" [];
      in_transaction := false
    end in

  let () = begin_transaction () in

object (self)
  method mem key =
    match Db.sql1 db "SELECT count(*) FROM blobs WHERE oid = ?"
        [Db.Data.BLOB (Hash.get_raw key)]
    with
      | [| Db.Data.INT 0L |] -> false
      | [| Db.Data.INT _ |] -> true
      | _ -> raise (TODO "Unknown result from 'mem' call")

  method find key =
    match self#find_full key with
      | None -> raise Not_found
      | Some (chunk, _info, _kind) -> chunk ()

  method find_option key =
    match self#find_full key with
      | None -> None
      | Some (chunk, _info, _kind) -> Some (chunk ())

  method find_full key =
    match Db.sqln db "SELECT kind, size, zsize, data FROM blobs WHERE oid = ?"
        [Db.Data.BLOB (Hash.get_raw key)]
    with
      | [] -> None
      | [[| Db.Data.TEXT kind; Db.Data.INT size; Db.Data.INT zsize; Db.Data.NULL |]] ->
          (* TODO: How to eliminate this redundancy *)
          let size = Int64.to_int size in
          let zsize = Int64.to_int zsize in
          let info () =
            { Chunk.in_hash = key;
              Chunk.in_kind = kind;
              Chunk.in_data_length = size;
              Chunk.in_write_size = 0 } in
          if size == zsize then
            let get_chunk () =
              let payload = read_chunk key zsize in
              Chunk.make_plain_chunk kind (Some key) payload in
            Some (get_chunk, info, kind)
          else
            let get_chunk () =
              let payload = read_chunk key zsize in
              Chunk.make_compressed_chunk kind (Some key) payload size in
            Some (get_chunk, info, kind)
      | [[| Db.Data.TEXT kind; Db.Data.INT size; Db.Data.INT zsize; Db.Data.BLOB payload |]] ->
          let size = Int64.to_int size in
          let zsize = Int64.to_int zsize in
          let info () =
            { Chunk.in_hash = key;
              Chunk.in_kind = kind;
              Chunk.in_data_length = size;
              Chunk.in_write_size = 0 } in
          if size == zsize then
            let get_chunk () = Chunk.make_plain_chunk kind (Some key) payload in
            Some (get_chunk, info, kind)
          else
            let get_chunk () =
              Chunk.make_compressed_chunk kind (Some key) payload size in
            Some (get_chunk, info, kind)
      | _ -> raise (TODO "Incorrect DB query result")

  method find_index _key = raise (TODO "find_index ")
  method get_backups =
    let rows = Db.sqln db "SELECT oid FROM blobs WHERE kind = 'back'" [] in
    List.map sql_to_hash rows

  method close =
    commit ();
    Db.close db

  method uuid = raise (TODO "uuid ")

  method add chunk =
    let zsize, zdata =
      match chunk#zdata with
        | None -> (chunk#data_length, chunk#data)
        | Some zdata ->
            (* Don't use the zdata unless it is actually smaller. *)
            if String.length zdata < chunk#data_length then
              (String.length zdata, zdata)
            else
              (chunk#data_length, chunk#data) in
    let payload = if zsize > 100000 then begin
      write_chunk chunk#hash zdata;
      Db.Data.NULL
    end else Db.Data.BLOB zdata in
    Db.sql0 db "INSERT OR FAIL INTO blobs (oid, kind, size, zsize, data) VALUES (?, ?, ?, ?, ?)"
      [ Db.Data.BLOB (Hash.get_raw chunk#hash);
        Db.Data.TEXT chunk#kind;
        Db.Data.INT (Int64.of_int chunk#data_length);
        Db.Data.INT (Int64.of_int zsize);
        payload ]

  method flush =
    commit ();
    begin_transaction ()
end
