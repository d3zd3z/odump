(* Interface to a storage database. *)

open Batteries_uni
open Printf

module Data = Sqlite3.Data

type schema_info = {
  schema_version : string;
  schema_text : string array
}

type t = Sqlite3.db

let must result =
  if result <> Sqlite3.Rc.OK then
    Log.failure ("sqlite error", ["code", Sqlite3.Rc.to_string result])

(* Perform an sql statement, installing arguments as appropriate. *)
let sql_fold db f a0 text args =
  let stmt = Sqlite3.prepare db text in
  List.iteri (fun num arg -> must (Sqlite3.bind stmt (num+1) arg)) args;
  let rec loop accum =
    begin match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
	let data = Sqlite3.row_data stmt in
	loop (f accum data)
      | Sqlite3.Rc.DONE -> accum
      | err ->
	Log.failure ("sqlite error", ["query", text;
				      "code", Sqlite3.Rc.to_string err])
    end in
  let result = loop a0 in
  must (Sqlite3.finalize stmt);
  result

(* Run the query, returning the results as a list. *)
let sqln db text args =
  let result = sql_fold db (fun a b -> b :: a) [] text args in
  List.rev result

(* Run a query, expecting no results. *)
let sql0 db text args =
  sql_fold db (fun _ x -> Log.failure ("not expecing SQL rows", ["query", text])) () text args

(* Run a query, expecting a single result. *)
let sql1 db text args =
  match sqln db text args with
    | [a] -> a
    | rows -> Log.failure ("expecting only a single row", ["count", string_of_int (List.length rows)])

(* Determine the schema version from this database. *)
let get_schema_version db =
  let fail () = Log.failure ("unexpected result from table query", []) in
  match sql1 db "select count(*) from sqlite_master where type = 'table' and name = 'schema_version'" [] with
    | [| Data.INT 0L |] -> None
    | [| Data.INT 1L |] ->
      begin match sql1 db "select version from schema_version" [] with
	| [| Data.TEXT version |] -> Some version
	| _ -> fail ()
      end
    | _ -> fail ()

let install_schema db info =
  sql0 db "begin transaction" [];
  sql0 db "create table schema_version (version text)" [];
  sql0 db "insert into schema_version values (?)" [Data.TEXT info.schema_version];
  Array.iter (fun query -> sql0 db query []) info.schema_text;
  sql0 db "commit" []

let connect path info =
  let db = Sqlite3.db_open path in

  begin match get_schema_version db with
    | None -> install_schema db info
    | Some version when version = info.schema_version -> ()
    | Some version ->
      Log.failure ("Schema mismatch on database", ["path", path;
						   "found", version;
						   "expect", info.schema_version])
  end;
  db

let close db =
  if not (Sqlite3.db_close db) then
    Log.failure ("Error closing database", [])
