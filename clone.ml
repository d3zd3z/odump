(* Cloning of trees *)

open Batteries_uni

class clone_visitor dest_pool =
object
  inherit Nodes.empty_visitor
  method! want_full_data = true
  method! leave _ chunk _ =
    if not (dest_pool#mem chunk#hash) then
      dest_pool#add chunk
end

let clone_trees src_pool dest_pool hashes =
  let each hash =
    Log.with_output (fun () ->
      Printf.eprintf "Cloning: %s\n" (Hash.to_string hash));
    Nodes.walk src_pool "." hash (new clone_visitor dest_pool) in
  List.iter each hashes
