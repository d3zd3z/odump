(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Visualization of Caml objects. *)

(* This module provides a graphic visualization of Objective Caml objects,
   whatever they are (it is a polymorphic function).
   The representation is output in an ASCII file, in a suitable format
   for the \emph{dot} tool 
   (see \texttt{http://www.research.att.com/sw/tools/graphviz}).
   If [f] is the output file, then you can, for instance, produce a PostScript
   document from it with the following command
   $$\texttt{dot -Tps -o $f$.ps $f$}$$

   The only exported function is [display], which take the name of the
   output file, and the Caml object to display.
   [display_list] is a variant displaying all the elements of a list, 
   but not the list itself. 
*)

val display : string -> 'a -> unit

val display_list : string -> 'a list -> unit
