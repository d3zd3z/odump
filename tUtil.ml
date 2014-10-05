(* Test utilities. *)

open Batteries

(* Similar to base64, but filename safe *)
let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

(* Local random state just for the filenames. *)
let rnd = Random.State.make
  [| Unix.getpid (); truncate (Unix.time ()) |]

let random_name base =
  let buf = Buffer.create (String.length base + 16) in
  Buffer.add_string buf base;
  for _i = 0 to 15 do
    Buffer.add_char buf chars.[Random.State.int rnd 64]
  done;
  Buffer.contents buf

(* Can be cleared to keep temp directories on exit for debugging. *)
let do_cleanup = ref true

let rec cleanup path =
  if !do_cleanup then begin
    let names = Sys.readdir path in
    let each name =
      let child = Filename.concat path name in
      let stats = Unix.lstat child in
      if stats.Unix.st_kind = Unix.S_DIR then
	cleanup child
      else
	Unix.unlink child in
    Array.iter each names;
    Unix.rmdir path
  end else
    Printf.printf "[Leaving '%s' for debugging]\n" path

let with_temp_dir op () =
  let dir = Filename.get_temp_dir_name () in
  let rec loop count =
    if count = 0 then failwith "Unable to create temp dir";
    let name = Filename.concat dir (random_name "odump-") in
    try
      Unix.mkdir name 0o755;
      name
    with
	Unix.Unix_error _ -> loop (count - 1) in
  let path = loop 5 in
  with_dispose ~dispose:cleanup op path

let with_temp_pool op =
  let op2 dir =
    File_pool.create_file_pool dir;
    with_dispose ~dispose:(fun p -> p#close) (fun p -> op dir p)
      (File_pool.open_file_pool dir) in
 with_temp_dir op2

(* For now, assume there are no spaces in the names. *)
let copy_file src dest =
  let command = Printf.sprintf "cp %s %s" src dest in
  match Sys.command command with
    | 0 -> ()
    | n -> Log.failf "Unable to copy file src=%S, dest=%S, code=%d" src dest n

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

let make_random_chunk ?(kind="blob") size index =
  Chunk.chunk_of_string kind (make_random_string size index)
