(* Figfs command parser/structures.
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

open Util

type cmd =
  | CreateWorkspace of string * string
  | DestroyWorkspace of string

let string_of_cmd (cmd:cmd) : string =
  match cmd with
  | CreateWorkspace (w, h) -> Printf.sprintf "create workspace %s %s" w h
  | DestroyWorkspace w -> Printf.sprintf "destroy workspace %s" w

let cmd_of_string (cmd:string) : cmd option =
  let words = unwords cmd in
  match words with
  | ["create"; "workspace"; name; hash] ->
      if String.length hash = 40
      then Some (CreateWorkspace (name, hash))
      else None
  | ["destroy"; "workspace"; name] ->
    Some (DestroyWorkspace name)
  | _ -> None

