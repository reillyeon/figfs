open Repository
open Object
open Sha1file

let worklist : string list ref = ref []

let _ =
  Arg.parse [] (fun f -> worklist := f :: !worklist) "cat-file";
  set_repo_dir ".";
  List.iter (fun f -> print_endline (string_of_obj (read_sha1_file f)))
    !worklist;
