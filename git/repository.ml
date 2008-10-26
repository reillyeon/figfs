let repo_dir : string option ref = ref None

(* Return the repository location, or raise an exception if one hasn't been
 * defined yet. *)
let get_repo_dir (x:unit) : string =
  ignore x;
  match !repo_dir with
  | Some dir -> dir
  | None -> failwith "No repository has been defined."

(* Sets the repository directory. *)
let set_repo_dir (dir:string) : unit =
  repo_dir := Some dir
