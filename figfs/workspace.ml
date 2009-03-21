(* Figfs writable workspace management.
 * Copyright (C) 2009 Reilly Grant
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

open Repository
open Util

module StringSet = Set.Make(String)

type workspace = {
    w_name : string;
    w_tree : string;
    mutable w_whiteout : StringSet.t
  }

let dir () =
  String.concat "/" [get_repo_dir (); ".figfs"; "workspaces"]

let file_dir (workspace:string) =
  String.concat "" [(dir ()); "/"; workspace; ".dir"]

let file_path (workspace:string) (path:string) =
  String.concat "" [(file_dir workspace); path]

let workspaces : (string, workspace) Hashtbl.t = Hashtbl.create 8

let parse_config () =
  let config_file = Filename.concat (dir ()) "config" in
  if Sys.file_exists config_file then
    let file = open_in config_file in
    try (
      while true do
        let line = input_line file in
        let words = unwords line in
        match words with
        | [hash; name] -> Hashtbl.add workspaces name
              { w_name = name; w_tree = hash; w_whiteout = StringSet.empty }
        | _ -> failwith "Parse error in workspace config."
      done
    ) with End_of_file -> ()
  else ((* no config file, that's ok *))

let save_config () : unit =
  let config_file = Filename.concat (dir ()) "config" in
  let file = open_out config_file in
  Hashtbl.iter (fun _ w ->
    Printf.fprintf file "%s %s\n" w.w_tree w.w_name
  ) workspaces

let init () : unit =
  let workspace_dir = dir () in
  if Sys.file_exists workspace_dir then (
    if Sys.is_directory workspace_dir then (
      parse_config ()
    ) else (
      failwith "Cannot create workspace directory."
    )
  ) else (
    Unix.mkdir workspace_dir 0o755;
  )

let list () : string list =
  Hashtbl.fold (fun _ w l -> w.w_name :: l) workspaces []

let create (name:string) (hash:string) : unit =
  Hashtbl.add workspaces name { w_name = name; w_tree = hash;
                                w_whiteout = StringSet.empty };
  Unix.mkdir (file_dir name) 0o755;
  save_config ()

let destroy (name:string) : unit =
  failwith "Workspace.destroy"

let file_exists (workspace:string) (path:string) (default:bool) : bool =
  let w = Hashtbl.find workspaces workspace in
  if StringSet.mem path w.w_whiteout then false
  else if Sys.file_exists (file_path workspace path) then true
  else default

let create_file (workspace:string) (path:string) : unit =
  failwith "Workspace.create_file"

let delete_file (workspace:string) (path:string) : unit =
  failwith "Workspace.delete_file"

let base_commit (workspace:string) : string =
  let w = Hashtbl.find workspaces workspace in
  w.w_tree
