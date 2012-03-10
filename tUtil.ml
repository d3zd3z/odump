(* Test utilities. *)

(* Similar to base64, but filename safe *)
let chars =
  [| 'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';
     'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'a';'b';'c';'d';'e';'f';
     'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';
     'w';'x';'y';'z';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'-';'_'
  |]

(* Local random state just for the filenames. *)
let rnd = Random.State.make
  [| Unix.getpid (); truncate (Unix.time ()) |]

let random_name base =
  let buf = Buffer.create (String.length base + 16) in
  Buffer.add_string buf base;
  for i = 0 to 15 do
    Buffer.add_char buf chars.(Random.State.int rnd 64)
  done;
  Buffer.contents buf

(* Can be cleared to keep temp directories on exit for debugging. *)
let do_cleanup = ref true

let rec cleanup path =
  if !do_cleanup then begin
    let names = Sys.readdir path in
    let each name =
      let child = Filename.concat path name in
      let stats = Unix.lstat child in
      if stats.Unix.st_kind = Unix.S_DIR then
	cleanup child
      else
	Unix.unlink child in
    Array.iter each names;
    Unix.rmdir path
  end else
    Printf.printf "[Leaving '%s' for debugging]\n" path

let with_temp_dir op () =
  let dir = Filename.temp_dir_name in
  let rec loop count =
    if count = 0 then failwith "Unable to create temp dir";
    let name = Filename.concat dir (random_name "odump-") in
    try
      Unix.mkdir name 0o755;
      name
    with
	Unix.Unix_error _ -> loop (count - 1) in
  let path = loop 5 in
  BatStd.with_dispose ~dispose:cleanup op path
