(* Backup chunks *)

(* Chunks are the fundamental unit of backup.  Everything is made out
 * of, or broken up into chunks.
 * 
 * Each chunk consists of a 4-character kind field (4 8-bit characters,
 * meaning they should really only be 7-bit characters to avoid
 * encoding problems), and zero or more bytes of data.
 * 
 * Chunks inherently support compression of their payload, and are
 * handled in both compressed and uncompressed form.  Generally, the
 * uncompressed format will represent the "real" backup data, and the
 * compressed version will be used for network transfer or the storage
 * pool. *)

(* Compression/decompression of buffers. *)

let uncompress src dest_len =
  let dest = String.create dest_len in
  let src_offset = ref 0 in
  let dest_offset = ref 0 in
  let refill buf =
    let count = min (String.length buf) (String.length src - !src_offset) in
    String.blit src !src_offset buf 0 count;
    src_offset := !src_offset + count;
    count in
  let flush buf count =
    if !dest_offset + count > dest_len then failwith "uncompress overflow";
    String.blit buf 0 dest !dest_offset count;
    dest_offset := !dest_offset + count in
  Zlib.uncompress ~header:true refill flush;
  dest

(* Each chunk contains a header
 *  offset  length  field
 *       0      16  chunk-magic
 *      16       4  compressed length, amount stored in file.
 *      20       4  uncompress length, or -1 for not compressed
 *      24       4  kind
 *      28      20  sha1 of type + uncompressed-data
 *      48     clen data
 *            0-15  padding
 *
 * The numbers are always represented in little endian, and the whole
 * chunk is padded to a multiple of 16 bytes. *)

open Printf
let finally = BatStd.finally

let pool_magic = "adump-pool-v1.1\n"

let read_buffer channel len =
  let buf = String.create len in
  really_input channel buf 0 len;
  buf

let get32le buf offset =
  let ch pos = Char.code buf.[offset + pos] in
  ch 0 lor
    (ch 1 lsl 8) lor
    (ch 2 lsl 16) lor
    (ch 3 lsl 24)

type header = {
  h_clen: int;
  h_len: int;
  h_kind: string;
  h_hash: Hash.t
}

let get_header chan =
  let buf = read_buffer chan 48 in
  let magic = String.sub buf 0 16 in
  if magic <> pool_magic then failwith "Invalid magic"; (* TODO: Proper exception. *)
  let clen = get32le buf 16 in
  let len = get32le buf 20 in
  let kind = String.sub buf 24 4 in
  let hash = String.sub buf 28 20 in
  { h_clen = clen; h_len = len; h_kind = kind; h_hash = Hash.of_raw hash }

let get_payload chan len =
  let payload = read_buffer chan len in
  let pad_count = 15 land (-len) in
  if pad_count > 0 then begin
    let _ = read_buffer chan pad_count in
    ()
  end;
  payload

let test_file path =
  let fd = open_in_bin path in
  let closer () = close_in fd in
  finally closer (fun fd ->
    let header = get_header fd in
    printf "len=%d, clen=%d, kind=%s\n" header.h_len header.h_clen header.h_kind;
    let payload = get_payload fd header.h_clen in
    let payload = if header.h_len > 0 then
	uncompress payload header.h_len
      else
	payload in
    printf "hash: %s\n" (Hash.to_string header.h_hash);
    let h2 = Hash.of_data [ header.h_kind; payload ] in
    printf "hash: %s\n" (Hash.to_string h2);
    (* Pdump.pdump payload *)
  ) fd
  (* Pdump.pdump payload *)

let _ = test_file "p/pool-data-0000.data"

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

let main () =
  for i = 1 to 99 do
    Printf.printf "[%3d]: '%s'\n" i (make_random_string i i)
  done
(* let _ = main () *)
