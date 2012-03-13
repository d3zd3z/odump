(* Property files *)

open Batteries_uni
open Printf

module StringMap = Map.StringMap

type t = string StringMap.t

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

let config = Pxp_types.default_config
let spec = Pxp_tree_parser.default_spec

let catalog =
  new Pxp_reader.lookup_id_as_string
    [ Pxp_types.System(sun_properties_url), sun_properties_dtd; ]

let get_entry node =
  match node#attribute "key" with
    | Pxp_types.Value x -> x
    | _ -> failwith "Invalid 'key' attribute in XML"

let decode_properties doc =
  let each map node = StringMap.add (get_entry node) node#data map in
  let entries = Pxp_document.find_all_elements "entry" doc#root in
  List.fold_left each StringMap.empty entries

(* Fixenc may be necessary of not specified. *)
let of_java_xml text =
  let source = Pxp_types.from_string ~alt:[catalog] text in
  let doc = Pxp_tree_parser.parse_document_entity config source spec in
  decode_properties doc
