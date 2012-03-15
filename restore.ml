(* Backup restoration. *)

class restore_visitor dest =
  let dest_len = String.length dest in
object
  inherit Nodes.empty_visitor

  val mutable out_descr = None

  method want_full_data = true

  method! enter path chunk node = match node with
    | Nodes.NodeNode ("DIR", props) ->
      if String.length path > dest_len then
	Unix.mkdir path 0o755
    | Nodes.NodeNode ("REG", props) ->
      out_descr <- Some (Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o644)

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
	  Unix.close fd
      end
    | _ -> ()
end

let run_restore pool root dest =
  Misc.ensure_empty_directory ~what:"restore" dest;
  Nodes.walk pool dest root (new restore_visitor dest)
