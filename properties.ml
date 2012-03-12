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

(* Update an xmlparser to resolve the given dtd at the given url. *)
let set_dtd_resolver pars url dtd_text =
  let dtd = Dtd.parse_string dtd_text in
  let checked = Dtd.check dtd in
  let resolver name =
    if name = url then checked
    else raise (Xml.File_not_found name) in
  XmlParser.resolve pars resolver

(* Given XML whose children contain pcdata, put them into a string. *)
let get_data xml =
  let buf = Buffer.create 16 in
  let each xml = Buffer.add_string buf **> Xml.pcdata xml in
  Xml.iter each xml;
  Buffer.contents buf

let of_java_xml text =
  let pars = XmlParser.make () in
  set_dtd_resolver pars sun_properties_url sun_properties_dtd;
  let single map xml =
    match Xml.tag xml with
	"comment" -> map
      | "entry" -> StringMap.add (Xml.attrib xml "key") (get_data xml) map
      | tag -> failwith **> sprintf "Unknown tag in property: '%s'" tag
  in Xml.fold single StringMap.empty **> XmlParser.parse pars (XmlParser.SString text)
