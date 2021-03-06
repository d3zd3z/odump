(* Sha1 hashes. *)

open Batteries

type t = string

let of_data lst =
  let h = Cryptokit.Hash.sha1 () in
  List.iter h#add_string lst;
  h#result

let of_raw item =
  if String.length item <> 20 then Log.failf "Hash must be 20 bytes: %S" item;
  String.copy item

(* Note that this doesn't copy the string, so be careful. *)
let get_raw item = item

let to_string hash =
  let buf = Buffer.create (2 * String.length hash) in
  let add ch =
    Buffer.add_string buf (Printf.sprintf "%02x" (Char.code ch)) in
  String.iter add hash;
  Buffer.contents buf

let of_string text =
  if String.length text <> 40 then Log.failf "Expect 40 character string: %S" text;
  let result = String.create 20 in
  let get pos = int_of_string ("0x" ^ String.sub text (pos*2) 2) in
  for i = 0 to 19 do
    result.[i] <- Char.chr (get i)
  done;
  result

let hash_change (--) sentinel hash =
  let hash = String.copy hash in
  let rec loop pos =
    if pos < 0 then () else begin
      hash.[pos] <- Char.chr ((Char.code hash.[pos] -- 1) land 255);
      if hash.[pos] = sentinel then loop (pos - 1)
    end
  in loop 19;
  hash

let succ = hash_change (+) '\x00'
let pred = hash_change (-) '\xff'

let compare = String.compare

let peek_byte hash pos = Char.code hash.[pos]

let null_hash = of_raw (String.make 20 '\x00')
