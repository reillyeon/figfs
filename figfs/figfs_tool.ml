(* Figfs command line utility.
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

let usage () : unit =
  let msg = String.concat "\n" [
    "Usage: figfs command [args]";
    "";
    "Commands:";
    "  help                         Display this message.";
    "  mount [options] <mountpoint> Mount figfs.";
    "  create <workspace> <commit>  Create a new workspace.";
    "  destroy <workspace>          Destroy a workspace.";
    ""] in
  Printf.eprintf "%s\n" msg

let check_figfs () : unit =
  if not (Sys.file_exists ".figfs_ctrl")
  then failwith "Not on a figfs filesystem."

let mount () : unit =
  let args = Array.append [|"mount.figfs"|]
      (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  Figfs.main args

let create_workspace () : unit =
  if Array.length Sys.argv != 4
  then failwith "Usage: figfs create <workspace>"
  else (
    check_figfs ();
    let ctrl = open_out ".figfs_ctrl" in
    Printf.fprintf ctrl "create workspace %s %s\n" Sys.argv.(2) Sys.argv.(3);
    close_out ctrl
  )

let destroy_workspace () : unit =
  if Array.length Sys.argv != 3
  then failwith "Usage: figfs destroy <workspace>"
  else (
    check_figfs ();
    let ctrl = open_out ".figfs_ctrl" in
    Printf.fprintf ctrl "destroy workspace %s\n" Sys.argv.(2);
    close_out ctrl
  )

let _ =
  if Array.length Sys.argv < 2 then usage ()
  else try (
    match Sys.argv.(1) with
    | "help"    -> usage ()
    | "mount"   -> mount ()
    | "create"  -> create_workspace ()
    | "destroy" -> destroy_workspace ()
    | s         -> Printf.eprintf "Unknown command '%s'.\n\n" s; usage ()
  ) with Failure s -> Printf.eprintf "%s\n" s
