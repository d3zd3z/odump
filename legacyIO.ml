(* Allow Legacy I/O while still using the main Batteries library.

   Intended use is to
   [open LegacyIO]
   which will shadow the IO operations with the legacy versions. *)

open Batteries_uni

type in_channel = Legacy.in_channel
let open_in_bin = Legacy.open_in_bin
let close_in = Legacy.close_in
let really_input = Legacy.really_input

type out_channel = Legacy.out_channel
let output_string = Legacy.output_string
let output_byte = Legacy.output_byte
let out_channel_length = Legacy.out_channel_length
let open_out_gen = Legacy.open_out_gen
let open_out_bin = Legacy.open_out_bin
let flush = Legacy.flush
let pos_out = Legacy.pos_out
let seek_out = Legacy.seek_out
let close_out = Legacy.close_out
