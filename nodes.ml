(* Backup nodes. *)

open Batteries_uni
open Printf

module StringMap = Map.StringMap

type node =
  | BackupNode of float * string StringMap.t
  | OtherNode

let get pool hash =
  let chunk = pool#find hash in
  match chunk#kind with
    | "back" ->
      let props = Properties.of_java_xml chunk#data in
      let date = (float_of_string **> StringMap.find "_date" props) /. 1000.0 in
      let props = StringMap.remove "_date" props in
      BackupNode (date, props)
    | kind -> failwith **> sprintf "Unknown node kind: '%s'" kind
