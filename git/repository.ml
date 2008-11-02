(* Repository configuration.
 * Copyright (C) 2008 Reilly Grant
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *)

let repo_dir : string option ref = ref None

(* Return the repository location, or raise an exception if one hasn't been
 * defined yet. *)
let get_repo_dir () : string =
  match !repo_dir with
  | Some dir -> dir
  | None -> failwith "No repository has been defined."

(* Sets the repository directory. *)
let set_repo_dir (dir:string) : unit =
  let git_objects = List.fold_left Filename.concat dir [".git"; "objects"] in
  if Sys.file_exists git_objects && Sys.is_directory git_objects
  then repo_dir := Some (Filename.concat dir ".git")
  else
    let objects = Filename.concat dir "objects" in
    if Sys.file_exists objects && Sys.is_directory objects
    then repo_dir := Some dir
    else failwith ("Does not appear to be a Git repository: " ^ dir)

let find_repo () : string option =
  let rec helper dir =
    let git_objects = List.fold_left Filename.concat dir [".git"; "objects"] in
    if Sys.file_exists git_objects && Sys.is_directory git_objects
    then Some dir
    else
      let objects = Filename.concat dir "objects" in
      if Sys.file_exists objects && Sys.is_directory objects
      then Some dir
      else
        let parent_dir = Filename.dirname dir in
        if parent_dir = dir then None
        else helper parent_dir
  in helper (Sys.getcwd ())
