open Repository
open Object
open Manager

let worklist : string list ref = ref []

let _ =
  Arg.parse [] (fun f -> worklist := f :: !worklist) "cat-file";
  match find_repo () with
  | Some dir ->
      set_repo_dir dir;
      List.iter (fun f -> print_endline (string_of_obj (find_object f)))
        !worklist
  | None ->
      print_endline "Not a Git repository."
