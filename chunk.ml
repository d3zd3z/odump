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
  if !dest_offset <> dest_len then failwith "uncompress underflow";
  dest

(* Try compressing a block of data.  Return (Some bytes) if the
 * compressed is smaller than the uncompressed data, or None if the
 * compression would make it larger. *)
let compress src =
  let dest_len = String.length src in
  let dest = String.create dest_len in
  let src_offset = ref 0 in
  let dest_offset = ref 0 in
  let refill buf =
    let count = min (String.length buf) (String.length src - !src_offset) in
    String.blit src !src_offset buf 0 count;
    src_offset := !src_offset + count;
    count in
  let flush buf count =
    (* Discard overflows, since we're going to discard the whole
     * result, anyway. *)
    let count = min count (dest_len - !dest_offset) in
    String.blit buf 0 dest !dest_offset count;
    dest_offset := !dest_offset + count in
  Zlib.compress refill flush;
  if !dest_offset < dest_len then
    Some (String.sub dest 0 !dest_offset)
  else
    None

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

let pool_magic = "adump-pool-v1.1\n"

let put32le buf offset value =
  let put pos num = buf.[offset + pos] <- Char.chr (num land 255) in
  put 0 value;
  put 1 (value lsr 8);
  put 2 (value lsr 16);
  put 3 (value lsr 24)

type header = {
  h_clen: int;
  h_len: int;
  h_kind: string;
  h_hash: Hash.t
}

let write_header chan header =
  let buffer = String.create 48 in
  String.blit pool_magic 0 buffer 0 16;
  put32le buffer 16 header.h_clen;
  put32le buffer 20 header.h_len;
  String.blit header.h_kind 0 buffer 24 4;
  String.blit (Hash.get_raw header.h_hash) 0 buffer 28 20;
  output_string chan buffer

class type chunk =
object
  method kind: string
  method data: string
  method data_length: int
  method hash: Hash.t
  method zdata: string option
  method write_size: int
  method write: out_channel -> int
end

class virtual base_chunk =
object (self)
  method virtual hash : Hash.t
  method virtual kind : string
  method virtual data : string
  method virtual zdata : string option
  method virtual data_length : int
  method write_size =
    let clen = match self#zdata with
	None -> self#data_length
      | Some p -> String.length p in
    let padding = 15 land (-clen) in
    48 + clen + padding

  method write chan =
    let pos = pos_out chan in
    let (payload, uclen) = match self#zdata with
	None -> (self#data, -1)
      | Some p -> (p, self#data_length) in
    let clen = String.length payload in
    let header = { h_clen = clen;
		   h_len = uclen;
		   h_kind = self#kind;
		   h_hash = self#hash } in
    write_header chan header;
    output_string chan payload;
    let padding = 15 land (-clen) in
    if padding > 0 then
      output_string chan (String.make padding '\x00');
    pos
end

class plain_chunk kind data =
  let hash = lazy (Hash.of_data [kind; data]) in
  let zdata = lazy (compress data) in
object
  inherit base_chunk
  method hash = Lazy.force hash
  method data = data
  method data_length = String.length data
  method zdata = Lazy.force zdata
  method kind = kind
end

class compressed_chunk kind zdata data_length =
  let data = lazy (uncompress zdata data_length) in
  let hash = lazy (Hash.of_data [kind; Lazy.force data]) in
object
  inherit base_chunk
  method hash = Lazy.force hash
  method data = Lazy.force data
  method data_length = data_length
  method zdata = Some zdata
  method kind = kind
end

let chunk_of_string kind data = new plain_chunk kind data


open Printf
let finally = BatStd.finally

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

(* let _ = test_file "p/pool-data-0000.data" *)
