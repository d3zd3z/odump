(* Binary support. *)

open Batteries
open LegacyIO

let get32le buf offset =
  let ch pos = Int32.of_int (Char.code buf.[offset + pos]) in
  let tmp = Int32.logor (ch 0)
    (Int32.logor (Int32.shift_left (ch 1) 8)
       (Int32.logor (Int32.shift_left (ch 2) 16)
          (Int32.shift_left (ch 3) 24))) in
  (* Make sure that negatives are still negative, even on 64-bit platforms. *)
  let tmp = Int32.to_int tmp in
  if tmp > 0x3FFFFFFF then tmp - (1 lsl 31) else tmp

let get16be buf offset =
  let ch pos = Char.code buf.[offset + pos] in
  ((ch 0) lsl 8) lor (ch 1)

let put32le buf offset value =
  let put pos num = buf.[offset + pos] <- Char.chr (num land 255) in
  put 0 value;
  put 1 (value lsr 8);
  put 2 (value lsr 16);
  put 3 (value lsr 24)

let put16be buf offset value =
  let put pos num = buf.[offset + pos] <- Char.chr (num land 255) in
  put 0 (value lsr 8);
  put 1 value

let buffer_add_16be buffer value =
  Buffer.add_char buffer (Char.chr ((value lsr 8) land 255));
  Buffer.add_char buffer (Char.chr (value land 255))

let read_buffer chan len =
  let buf = String.create len in
  really_input chan buf 0 len;
  buf
