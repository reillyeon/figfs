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
