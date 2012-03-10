(** {1 Backup chunks} *)

(**
   Chunks are the fundamental unit of backup.  Everything is made out
   of, or broken up into chunks.

   Each chunk consists of a 4-character kind field (4 8-bit
   characters, meaning they should really only be 7-bit characters to
   avoid encoding problems), and zero or more bytes of data.

   Chunks inherently support compression of their payload, and are
   handled in both compressed and uncompressed form.  Generally, the
   uncompressed format will represent the {e real} backup data, and
   the compressed version will be used for network transfer or the
   storage pool. *)

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

val chunk_of_string: string -> string -> chunk
(** [Chunk.chunk_of_string kind str] Create a chunk out of a kind and
    a string. *)

type info = {
  in_hash: Hash.t;
  in_kind: string;
  in_data_length: int;
  in_write_size: int }

val header_size: int
val read_info: in_channel -> info
(** Read the header of the next chunk in the channel.  Returns the
    info data about it. *)

val read: in_channel -> chunk
(** Read the next chunk from the given channel. *)

(** {2 Internal testing}
    These routines aren't intended to be used by normal clients, but
    are exposed to help with unit testing. *)

val uncompress: string -> int -> string
(** [Chunk.uncompress str len] uncompress the compressed data in
    [str], returning a buffer of [len] bytes.  Will fail if the
    uncompressed data is not exactly [len] bytes. *)

val compress: string -> string option
(** [Chunk.compress str] tries to compress the given string.  Will
    return [Some buf] if the data could be compressed.  If the
    compressed result wouldn't be smaller than the source, returns
    [None]. *)
