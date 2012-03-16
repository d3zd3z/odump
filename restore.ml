(* Backup restoration. *)

open Batteries_uni

module SM = Map.StringMap
module I64M = Map.Make(Int64)

class restore_visitor dest =
  let dest_len = String.length dest in
object
  inherit Nodes.empty_visitor

  val mutable out_descr = None

  (* Map from inode numbers to first path to reference them. *)
  val mutable hard_links = I64M.empty

  method want_full_data = true


  method! enter path chunk node = match node with
    | Nodes.NodeNode ("DIR", props) ->
      if String.length path > dest_len then
	Unix.mkdir path 0o755
    | Nodes.NodeNode ("REG", props) ->
      let nlink = int_of_string (SM.find "nlink" props) in
      let ino = Int64.of_string (SM.find "ino" props) in
      if nlink == 1 || not (I64M.mem ino hard_links) then begin
	out_descr <- Some (Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o644);
	if nlink > 1 then
	  hard_links <- I64M.add ino path hard_links
      end else begin
	Unix.link (I64M.find ino hard_links) path;
	raise Nodes.Prune
      end

    | Nodes.BlobNode data ->
      begin match out_descr with
	| None -> ()  (* Hardlink *)
	| Some fd ->
	  let len = String.length data in
	  let count = Unix.write fd data 0 len in
	  if count <> len then failwith "Short write"
      end
    | _ -> ()

  method! leave path chunk node = match node with
    | Nodes.NodeNode ("REG", props) ->
      begin match out_descr with
	| None -> () (* Create hardlink *)
	| Some fd ->
	  Unix.close fd;
	  out_descr <- None
      end
    | _ -> ()
end

let run_restore pool root dest =
  Misc.ensure_empty_directory ~what:"restore" dest;
  Nodes.walk pool dest root (new restore_visitor dest)