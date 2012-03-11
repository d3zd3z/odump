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

open Batteries_uni
open LegacyIO
open Binary

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
let header_size = 48

type header = {
  h_clen: int;
  h_len: int;
  h_kind: string;
  h_hash: Hash.t
}

let write_header chan header =
  let buffer = String.create header_size in
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
  (* TODO: The return result from write isn't actually needed. *)
end

type t = chunk

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
    header_size + clen + padding

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

type info = {
  in_hash: Hash.t;
  in_kind: string;
  in_data_length: int;
  in_write_size: int }

let get_header chan =
  let buf = read_buffer chan header_size in
  let magic = String.sub buf 0 16 in
  if magic <> pool_magic then failwith "Invalid magic"; (* TODO: Proper exception. *)
  let clen = get32le buf 16 in
  let len = get32le buf 20 in
  let kind = String.sub buf 24 4 in
  let hash = String.sub buf 28 20 in
  { h_clen = clen; h_len = len; h_kind = kind; h_hash = Hash.of_raw hash }

let read_info chan =
  let header = get_header chan in
  let padding = 15 land (-header.h_clen) in
  { in_hash = header.h_hash;
    in_kind = header.h_kind;
    in_data_length = if header.h_len = -1 then header.h_clen else header.h_len;
    in_write_size = header_size + header.h_clen + padding }

let read chan =
  let header = get_header chan in
  let data = read_buffer chan header.h_clen in
  let chunk = if header.h_len = -1 then
    new plain_chunk header.h_kind data
  else
    new compressed_chunk header.h_kind data header.h_len in
  if true then begin
    (* Verify the hash. *)
    if header.h_hash <> chunk#hash then failwith "Incorrect SHA1 reading chunk"
  end;
  chunk

(* {6 Files of chunks} *)

class type chunk_file =
object
  method read: int -> chunk
  (* method read_unchecked: int -> (chunk * Hash.t) *)
  method read_info: int -> info
  method append: chunk -> int
  method flush: unit
  method close: unit
end

class regular_chunk_file path =
object (self)
  val mutable reader = None
  val mutable writer = None
  val mutable size = 0
  val mutable dirty = false

  method private prepare_read =
    (* If we've been writing, make sure to flush. *)
    if dirty then begin
      self#flush;
      dirty <- false
    end;
    match reader with
	None ->
	  let r = open_in_bin path in
	  reader <- Some r;
	  r
      | Some r -> r

  method private prepare_write =
    dirty <- true;
    match writer with
	None ->
	  let w = open_out_gen [Open_wronly; Open_creat; Open_binary] 0o644 path in
	  writer <- Some w;
	  size <- out_channel_length w;
	  w
      | Some w -> w

  method read offset =
    let chan = self#prepare_read in
    seek_in chan offset;
    read chan

  method read_info offset =
    let chan = self#prepare_read in
    seek_in chan offset;
    read_info chan

  method append (chunk : chunk) =
    let chan = self#prepare_write in
    seek_out chan size;
    let pos = size in
    let _ = chunk#write chan in
    size <- size + chunk#write_size;
    pos

  method flush =
    match writer with
	None -> ()
      | Some chan -> flush chan

  method close =
    begin match writer with
	None -> ()
      | Some chan ->
	close_out chan;
	writer <- None
    end;
    begin match reader with
	None -> ()
      | Some chan ->
	close_in chan;
	reader <- None
    end

end

let open_chunk_file = new regular_chunk_file
