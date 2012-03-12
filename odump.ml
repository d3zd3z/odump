(* odump driver *)

open Batteries_uni
open Printf

module StringMap = Map.StringMap

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

let set_property_dtd p =
  let dtd = Dtd.parse_string sun_properties_dtd in
  let checked = Dtd.check dtd in
  let resolver name =
    match name with
	"http://java.sun.com/dtd/properties.dtd" -> checked
      | name -> raise (Xml.File_not_found name) in
  XmlParser.resolve p resolver

let get_data xml =
  let buf = Buffer.create 16 in
  let each xml = Buffer.add_string buf **> Xml.pcdata xml in
  Xml.iter each xml;
  Buffer.contents buf

let decode_properties xml =
  let single map xml =
    match Xml.tag xml with
	"comment" -> map
      | "entry" -> StringMap.add (Xml.attrib xml "key") (get_data xml) map
      | tag -> failwith **> sprintf "Unknown tag in property: '%s'" tag
  in Xml.fold single StringMap.empty xml

let dump_fmt = "%Y-%m-%d_%H:%M"

let format_date date =
  let tm = Unix.localtime date in
  sprintf "%04d-%02d-%02d %02d:%02d"
    (tm.Unix.tm_year + 1900)
    tm.Unix.tm_mon
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min

(* Note that CalendarLib doesn't deal well with timezones in the past. *)

let make_backup xml =
  let props = decode_properties xml in
  let date = (float_of_string **> StringMap.find "_date" props) /. 1000.0 in
  let props = StringMap.remove "_date" props in
object
  method show hash =
    let p = StringMap.enum (StringMap.remove "hash" props) in
    let buf = Buffer.create 32 in
    let one_prop (key, value) =
      Buffer.add_char buf ' ';
      Buffer.add_string buf key;
      Buffer.add_char buf '=';
      Buffer.add_string buf value in
    Enum.iter one_prop p;
    printf "%s %s%s\n"
      (Hash.to_string hash)
      (format_date date)
      (Buffer.contents buf)

  method date = date

  method date_compare other = compare date other#date
end

let main () =
  let pool = File_pool.open_file_pool "/mnt/grime/a64/pool-2011-11" in
  let backups = pool#get_backups in
  let pars = XmlParser.make () in
  set_property_dtd pars;
  let get hash =
    let chunk = pool#find hash in
    let xml = XmlParser.parse pars (XmlParser.SString chunk#data) in
    (hash, make_backup xml) in
  let backups = List.map get backups in
  let backups = List.sort ~cmp:(fun (_, a) (_, b) -> a#date_compare b) backups in
  List.iter (fun (hash, b) -> b#show hash) backups

let _ = main ()
