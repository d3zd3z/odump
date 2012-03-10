(* Hex dumping utility. *)

let pdump bytes =
  let len = String.length bytes in
  let hex = Buffer.create 49 in
  let ascii = Buffer.create 16 in
  let count = ref 0 in
  let total_count = ref 0 in
  let reset () =
    Buffer.reset hex;
    Buffer.reset ascii;
    count := 0 in
  let ship () =
    Printf.printf "%06x %-49s |%-16s|\n" (!total_count)
      (Buffer.contents hex)
      (Buffer.contents ascii);
    reset();
    total_count := !total_count + 16 in
  let rec add_byte pos =
    if pos < len then begin
      if !count = 16 then ship ();
      if !count = 8 then Buffer.add_char hex ' ';
      let ch = String.get bytes pos in
      Buffer.add_string hex (Printf.sprintf " %02x" (Char.code ch));
      Buffer.add_char ascii (if ch >= ' ' && ch <= '~' then ch else '.');
      count := !count + 1;
      add_byte (pos + 1)
    end in
  add_byte 0;
  if !count > 0 then ship ()
