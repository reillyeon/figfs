(* Git repository information *)

(** Set the repository location used by all library components. *)
val set_repo_dir : string -> unit

(** Get the repository location user by all library components. *)
val get_repo_dir : unit -> string

(** Traverse up the filesystem tree until a repository directory is found.
 * Returns None if the root is reached without finding a repository. *)
val find_repo : unit -> string option
