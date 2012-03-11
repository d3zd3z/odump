(* Binary support. *)

let get32le buf offset =
  let ch pos = Int32.of_int (Char.code buf.[offset + pos]) in
  let tmp = Int32.logor (ch 0)
    (Int32.logor (Int32.shift_left (ch 1) 8)
       (Int32.logor (Int32.shift_left (ch 2) 16)
          (Int32.shift_left (ch 3) 24))) in
  (* Make sure that negatives are still negative, even on 64-bit platforms. *)
  let tmp = Int32.to_int tmp in
  if tmp > 0x3FFFFFFF then tmp - (1 lsl 31) else tmp

let put32le buf offset value =
  let put pos num = buf.[offset + pos] <- Char.chr (num land 255) in
  put 0 value;
  put 1 (value lsr 8);
  put 2 (value lsr 16);
  put 3 (value lsr 24)

let read_buffer chan len =
  let buf = String.create len in
  really_input chan buf 0 len;
  buf
