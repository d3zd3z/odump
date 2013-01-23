(* Cloning of trees *)

open Batteries

class clone_meter =
object
  val start_time = Unix.gettimeofday ()
  val mutable count = 0L
  val mutable compressed = 0L
  val mutable uncompressed = 0L

  method add_node (chunk : Chunk.t) =
    count <- Int64.succ count;
    uncompressed <- Int64.add uncompressed (Int64.of_int chunk#data_length);
    compressed <- Int64.add compressed (Int64.of_int chunk#write_size)

  method aux_meter =
    let show fmt =
      let age = Unix.gettimeofday () -. start_time in
      Format.fprintf fmt "---- clone: %9Ld nodes written ----\n" count;
      Log.format_size_rate fmt " %s uncompressed (%s/sec)@\n" uncompressed age;
      Log.format_size_rate fmt " %s compressed   (%s/sec)@\n" compressed age in
    Log.build_format_meter show

end

class clone_visitor dest_pool meter =
object
  inherit Nodes.empty_visitor
  method! want_full_data = true
  method! enter _ chunk _ =
    if dest_pool#mem chunk#hash then
      raise Nodes.Prune
  method! leave _ chunk _ =
    if not (dest_pool#mem chunk#hash) then begin
      dest_pool#add chunk;
      meter#add_node chunk
    end
end

let clone_trees src_pool dest_pool hashes =
  let each hash =
    let meter = new clone_meter in
    let aux = meter#aux_meter in
    Log.with_output (fun () ->
      Printf.eprintf "Cloning: %s\n" (Hash.to_string hash));
    Nodes.walk ~aux_meter:aux src_pool "." hash (new clone_visitor dest_pool meter) in
  List.iter each hashes
