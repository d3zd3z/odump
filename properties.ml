(* Property files *)

open Batteries_uni

module StringMap = Map.StringMap

type t = string StringMap.t

(* Binary packed encoding of properties. *)
let decode_packed text =
  let len = String.length text in
  let kind_len = Char.code text.[0] in
  let kind = String.sub text 1 kind_len in
  let rec loop map pos =
    if pos = len then map else begin
      let klen = Char.code text.[pos] in
      let key = String.sub text (pos+1) klen in
      let pos = pos + 1 + klen in
      let vlen = Binary.get16be text pos in
      let value = String.sub text (pos+2) vlen in
      let pos = pos + 2 + vlen in
      loop (StringMap.add key value map) pos
    end in
  (kind, loop StringMap.empty (1 + kind_len))

let of_java_xml text =
  match decode_packed text with
    | ("back", props) -> props
    | (kind, _) -> Log.failf "Invalid kind of backup record: %S" kind

let of_jpool_xml text = decode_packed text
