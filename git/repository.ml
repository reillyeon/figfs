open Unix

let repo_dir : string option ref = ref None

(* Return the repository location, or raise an exception if one hasn't been
 * defined yet. *)
let get_repo_dir () : string =
  match !repo_dir with
  | Some dir -> dir
  | None -> failwith "No repository has been defined."

(* Sets the repository directory. *)
let set_repo_dir (dir:string) : unit =
  try
    access (dir ^ "/.git/objects") [X_OK];
    repo_dir := Some (dir ^ "/.git")
  with Unix_error _ ->
    try
      access (dir ^ "/objects") [X_OK];
      repo_dir := Some dir
    with Unix_error _ ->
      failwith ("Directory " ^ dir ^ " does not appear to be a Git repository.")

let find_repo () : string option =
  let rec helper dir =
    try
      access (dir ^ "/.git/objects") [X_OK];
      Some dir
    with Unix_error _ ->
      try
        access (dir ^ "/objects") [X_OK];
        Some dir
      with Unix_error _ ->
        if dir = "/" then None
        else helper (String.sub dir 0 (String.rindex dir '/'))
  in helper (getcwd ())
