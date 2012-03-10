open OUnit
open Printf
open TUtil

module L = BatList

(* Some utilities for randomly generating chunks. *)

type lrg = int32

(* This is biased a bit, but it doesn't really matter. *)
let random_next st limit =
  let st' = Int32.add (Int32.mul st 1103515245l) 12345l in
  let cur = Int32.rem (Int32.logand st' 0x7FFFFFFFl) (Int32.of_int limit) in
  (st', Int32.to_int cur)

let word_list =
  [| "the"; "be"; "to"; "of"; "and"; "a"; "in"; "that"; "have"; "I";
     "it"; "for"; "not"; "on"; "with"; "he"; "as"; "you"; "do"; "at";
     "this"; "but"; "his"; "by"; "from"; "they"; "we"; "say"; "her";
     "she"; "or"; "an"; "will"; "my"; "one"; "all"; "would"; "there";
     "their"; "what"; "so"; "up"; "out"; "if"; "about"; "who"; "get";
     "which"; "go"; "me"; "when"; "make"; "can"; "like"; "time"; "no";
     "just"; "him"; "know"; "take"; "person"; "into"; "year"; "your";
     "good"; "some"; "could"; "them"; "see"; "other"; "than"; "then";
     "now"; "look"; "only"; "come"; "its"; "over"; "think"; "also"
  |]

let random_word st =
  let (st', pos) = random_next st (Array.length word_list) in
  (st', word_list.(pos))

let make_random_string size index =
  let buf = Buffer.create (size + 10) in
  Buffer.add_string buf (Printf.sprintf "%d-%d" index size);
  let rec loop st =
    if Buffer.length buf >= size then
      Buffer.sub buf 0 size
    else begin
      Buffer.add_char buf ' ';
      let (st', word) = random_word st in
      Buffer.add_string buf word;
      loop st'
    end in
  loop (Int32.of_int index)

let compression () =
  assert_equal None (Chunk.compress (make_random_string 10 10));
  let comp len =
    let p1 = make_random_string len len in
    let c = match Chunk.compress p1 with
      | None -> assert_failure "Chunk didn't compress"
      | Some p -> p in
    let dc = Chunk.uncompress c len in
    (* Pdump.pdump p1;
       Pdump.pdump dc; *)
    assert_equal p1 dc in
  comp 256;
  comp (256*1024);
  ()

let simple () =
  let test len =
    let p1 = make_random_string len len in
    let ch = Chunk.chunk_of_string "blob" p1 in
    assert_equal p1 ch#data;
    assert_equal "blob" ch#kind;
    assert_equal len ch#data_length;
    assert_equal ch#hash (Hash.of_data ["blob"; p1]);
    match ch#zdata with
	None -> ()
      | Some comp -> assert_equal p1 (Chunk.uncompress comp len) in
  test 0;
  test 16;
  test 256;
  test (256 * 1024);
  ()

(* A list of integers from a to b, assuming a > min_integer. *)
let range a b =
  let rec loop p result =
    if p < a then result
    else loop (p-1) (p::result) in
  loop b []

let test_sizes () =
  let sizes = range 0 18 in
  let sizes = L.map (fun a -> let a = 1 lsl a in [a-1; a; a+1]) sizes in
  let sizes = L.filter (fun a -> a >= 0) (L.flatten sizes) in
  L.sort_unique compare sizes

(* Write a series of chunks to the given file, returning a list of
   pairs of 'pos', and the chunks. *)
let write_chunks chan =
  let old_pos = ref (pos_out chan) in
  let each size =
    let ch = Chunk.chunk_of_string "blob" (make_random_string size size) in
    let pos = ch#write chan in
    assert_equal pos !old_pos;
    assert_equal ch#write_size (pos_out chan - pos);
    old_pos := pos_out chan;
    (pos, ch) in
  List.map each (test_sizes ())

(* Given a list of (pos * chunk) pairs, and an input channel, verify
   that the info from read_info is correct. *)
let verify_info infos chan =
  let each (pos, ch) =
    seek_in chan pos;
    let info = Chunk.read_info chan in
    assert_equal info.Chunk.in_hash ch#hash;
    assert_equal info.Chunk.in_kind ch#kind;
    assert_equal info.Chunk.in_data_length ch#data_length;
    assert_equal info.Chunk.in_write_size ch#write_size in
  List.iter each infos

(* Verify the chunks themselves. *)
let verify_data infos chan =
  let each (pos, ch) =
    seek_in chan pos;
    let ch2 = Chunk.read chan in
    assert_equal ch#hash ch2#hash;
    assert_equal ch#data ch2#data;
    assert_equal ch#kind ch2#kind in
  List.iter each infos

(* Repeat of the above, using the chunk_file API *)
let cfile_write_chunks cfile =
  let each size =
    let ch = Chunk.chunk_of_string "blob" (make_random_string size size) in
    let pos = cfile#append ch in
    (pos, ch) in
  List.map each (test_sizes ())

let cfile_verify_chunks cfile infos =
  let each (pos, ch) =
    let info = cfile#read_info pos in
    assert_equal info.Chunk.in_hash ch#hash;
    assert_equal info.Chunk.in_kind ch#kind;
    assert_equal info.Chunk.in_data_length ch#data_length;
    assert_equal info.Chunk.in_write_size ch#write_size;
    let ch2 = cfile#read pos in
    assert_equal ch#hash ch2#hash;
    assert_equal ch#data ch2#data;
    assert_equal ch#kind ch2#kind in
  List.iter each infos

let io tmpdir =
  let name = Filename.concat tmpdir "foo.data" in
  let infos = BatStd.with_dispose ~dispose:close_out write_chunks (open_out_bin name) in
  BatStd.with_dispose ~dispose:close_in (verify_info infos) (open_in_bin name);
  BatStd.with_dispose ~dispose:close_in (verify_data infos) (open_in_bin name)

let file tmpdir =
  let name = Filename.concat tmpdir "file.data" in
  let process cfile =
    let infos = cfile_write_chunks cfile in
    cfile_verify_chunks cfile infos
  in
  BatStd.with_dispose ~dispose:(fun x -> x#close) process (new Chunk.regular_chunk_file name)

let suite = "chunk" >::: [
  "compression" >:: compression;
  "simple" >:: simple;
  "io" >:: with_temp_dir io;
  "file" >:: with_temp_dir file;
]
