(* Testing indirect nodes. *)

open Batteries_uni

open OUnit
open Printf
open TUtil

let int_chunk i = Chunk.chunk_of_string "blob" (string_of_int i)

class check_visitor =
object
  inherit Nodes.empty_visitor
  method! want_full_data = true

  val mutable count = 1

  method enter _ _ node = match node with
    | Nodes.BlobNode data ->
      assert_equal data (int_chunk count)#data;
      count <- count + 1
    (* | Nodes.IndirectNode (_, level, _) -> *)
    (*   Log.info (fun () -> ("indirect", ["level", string_of_int level])) *)
    | _ -> ()
end

let check_count pool count =
  let ind = Indirect.make_indirect pool "ind" 200 in
  for i = 1 to count do
    let chunk = int_chunk i in
    pool#add chunk;
    Indirect.add ind chunk#hash
  done;
  let top = Indirect.finish ind in

  let visitor = new check_visitor in
  Nodes.walk pool "." top visitor

let creation path pool =
  check_count pool 1;
  check_count pool 2;
  check_count pool 200;
  check_count pool 20000

let suite = "indirect" >::: [
  "creation" >:: with_temp_pool creation
]
