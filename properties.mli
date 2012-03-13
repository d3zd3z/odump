(* Properties are Maps that can be encoded in a small number of XML formats. *)

open Batteries_uni

type t = string Map.StringMap.t

val of_java_xml: string -> t
(** Decode a Java XML type of property. *)

val of_jpool_xml: string -> (string * t)
(** Decode a jpool xml-style property list (no DOCTYPE).  Returns the
    node kind and the property list. *)
