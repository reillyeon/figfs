(* cat-file utility.
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
