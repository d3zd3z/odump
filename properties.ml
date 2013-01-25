(* Property files *)

open Batteries

module StringMap = Maps.StringMap

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

(* Taken from http://java.sun.com/dtd/properties.dtd on 2012-03-11.
   Rather than fetching this every time, we're including it here, despite
   the copyright. *)
let sun_properties_url = "http://java.sun.com/dtd/properties.dtd"
let sun_properties_dtd = "\
<!--\n\
   Copyright 2006 Sun Microsystems, Inc.  All rights reserved.\n\
  -->\n\
\n\
<!-- DTD for properties -->\n\
\n\
<!ELEMENT properties ( comment?, entry* ) >\n\
\n\
<!ATTLIST properties version CDATA #FIXED \"1.0\">\n\
\n\
<!ELEMENT comment (#PCDATA) >\n\
\n\
<!ELEMENT entry (#PCDATA) >\n\
\n\
<!ATTLIST entry key CDATA #REQUIRED>\n\
"

(*
let jpool_dtd = "\
<!ELEMENT node ( entry* ) >\n\
<!ATTLIST node kind CDATA #REQUIRED>\n\
<!ELEMENT entry (#PCDATA)>\n\
<!ATTLIST entry key CDATA #REQUIRED>\n"
*)

let config = Pxp_types.default_config
let spec = Pxp_tree_parser.default_spec

let catalog =
  new Pxp_reader.lookup_id_as_string
    [ Pxp_types.System(sun_properties_url), sun_properties_dtd; ]

let get_attribute node key =
  match node#attribute key with
    | Pxp_types.Value x -> x
    | _ -> Log.failf "Invalid 'key' attribute in XML: %S" key

let decode_properties doc =
  let each map node = StringMap.add (get_attribute node "key") node#data map in
  let entries = Pxp_document.find_all_elements "entry" doc#root in
  List.fold_left each StringMap.empty entries

(* Fixenc may be necessary of not specified. *)
let of_java_xml' text =
  let source = Pxp_types.from_string ~alt:[catalog] text in
  let doc = Pxp_tree_parser.parse_document_entity config source spec in
  decode_properties doc

let of_java_xml text =
  if text.[0] = '<' then of_java_xml' text else
    match decode_packed text with
      | ("back", props) -> props
      | (kind, _) -> Log.failf "Invalid kind of backup record: %S" kind

let of_jpool_xml' text =
  let source = Pxp_types.from_string text in
  let doc = Pxp_tree_parser.parse_wfdocument_entity config source spec in

  (* TODO: Make this validation work.
  (* Validate against our DTD. *)
  let dtd_source = Pxp_types.from_string jpool_dtd in
  let dtd = Pxp_dtd_parser.parse_dtd_entity config dtd_source in
  dtd#set_root "node";
  let vroot = Pxp_marshal.relocate_document doc#root dtd spec in
  Pxp_document.validate vroot;
  *)

  let kind = get_attribute doc#root "kind" in
  (kind, decode_properties doc)

let of_jpool_xml text =
  if text.[0] = '<' then of_jpool_xml' text else
    decode_packed text
