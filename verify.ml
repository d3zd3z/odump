(* Verifying hashes *)

let verify files =
  Chunk.verify_hashes := true;
  let name = ref "" in
  let pos = ref 0 in
  let size = ref 0 in
  let start_time = ref (Unix.gettimeofday ()) in
  let show fmt =
    let age = Unix.gettimeofday () -. !start_time in
    Format.fprintf fmt "Verifying: %s@\n" !name;
    Log.format_size_rate fmt "  %s compressed (%s/sec)@\n" (Int64.of_int !pos) age;
    Log.format_size fmt "  (of %s)@." (Int64.of_int !size) in
  Log.set_meter (Log.build_format_meter show);
  let each file =
    name := file;
    pos := 0;
    start_time := Unix.gettimeofday ();
    let cf = Chunk.open_chunk_file file in
    size := cf#size;
    let rec loop offset =
      if offset < !size then begin
        pos := offset;
        Log.update_meter ();
        let next = cf#check offset in
        loop next
      end in
    loop 0;
    cf#close in
  List.iter each files;
  Log.finish_meter ()
