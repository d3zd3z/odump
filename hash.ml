(* Sha1 hashes. *)

type t = string

let of_data lst =
  let h = Cryptokit.Hash.sha1 () in
  List.iter h#add_string lst;
  h#result

let of_raw item =
  if String.length item <> 20 then failwith "Hash must be 20 bytes";
  String.copy item

let to_string hash =
  let buf = Buffer.create (2 * String.length hash) in
  let add ch =
    Buffer.add_string buf (Printf.sprintf "%02x" (Char.code ch)) in
  String.iter add hash;
  Buffer.contents buf

let of_string text =
  if String.length text <> 40 then failwith "Expect 40 character string.";
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

