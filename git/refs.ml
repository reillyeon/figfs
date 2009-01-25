(* References.
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

open Unix

type obj_ref = {
    r_name : string;               (* ref name *)
    r_target : Object.hash;        (* object referenced *)
    r_peeled : Object.hash option; (* commit referenced by the tag *)
  }

let string_of_obj_ref (ref:obj_ref) =
  let peeled =
    match ref.r_peeled with
    | Some s -> Printf.sprintf "Some %S" s
    | None -> "None" in
  Printf.sprintf "{ r_name = %S; r_target = %S; r_peeled = %s }"
    ref.r_name ref.r_target peeled

let packed_ref_cache : (string, obj_ref) Hashtbl.t =
  Hashtbl.create 8

(* If the mtime of get_repo_dir/packed-refs is greater than this, refresh the
   cache. *)
let packed_ref_mtime : float ref =
  ref 0.0

(* Check the mtime of the packed-refs file and refresh the cache if it is
   newer. *)
let refresh_packed_refs () : unit =
  let filename = Filename.concat (get_repo_dir ()) "packed-refs" in
  if Sys.file_exists filename then (
    let file_stat = Unix.stat filename in
    if file_stat.st_mtime > !packed_ref_mtime then (
      let fin = open_in filename in
      let buf = String.create (in_channel_length fin) in
      really_input fin buf 0 (String.length buf);
      close_in fin;
      let lines = split buf '\n' in
      let parse_line s =
        if starts_with "#" s then None
        else if String.length s < 42 then None
        else (
          let hash = String.sub s 0 40 in
          let name = String.sub s 41 (String.length s - 41) in
          Some { r_name = name; r_target = hash; r_peeled = None }
        ) in
      let rec helper lines =
        match lines with
        | line1 :: line2 :: rest -> (
            match parse_line line1 with
            | Some r ->
                if starts_with "^" line2 && String.length line2 = 41
                then (
                  let peeled = String.sub line2 1 40 in
                  Hashtbl.replace packed_ref_cache
                    r.r_name { r with r_peeled = Some peeled };
                  helper rest
                ) else (
                  Hashtbl.replace packed_ref_cache r.r_name r;
                  helper (line2 :: rest)
                )
            | None -> helper (line2 :: rest)
          )
        | line :: rest -> (
            match parse_line line with
            | Some r ->
                Hashtbl.replace packed_ref_cache r.r_name r;
                helper rest
            | None -> helper rest
          )
        | [] -> () in
      helper lines
    )
  )

(* Look up a reference by name in the packed references. *)
let get_packed_ref (ref:string) : obj_ref =
  refresh_packed_refs ();
  Hashtbl.find packed_ref_cache ref

let rec get_loose_ref (ref:string) : obj_ref =
  let ref_path = Filename.concat (get_repo_dir ()) ref in
  if not (Sys.file_exists ref_path)
  then raise Not_found
  else (
    let fin = open_in ref_path in
    let buf = String.create (in_channel_length fin) in
    really_input fin buf 0 (String.length buf);
    close_in fin;
    if starts_with "ref: " buf then (
      let ref = String.sub buf 5 (String.length buf - 6) in
      lookup ref
    ) else if String.length buf = 41 then (
      let hash = String.sub buf 0 (String.length buf - 1) in
      { r_name = ref; r_target = hash; r_peeled = None }
    ) else raise Not_found
  )

and lookup (ref:string) : obj_ref =
  try get_packed_ref ref
  with Not_found -> get_loose_ref ref

let get_packed_refs () : obj_ref list =
  refresh_packed_refs ();
  Hashtbl.fold (fun k v a -> v :: a) packed_ref_cache []

let get_loose_refs () : obj_ref list =
  let rec helper ref_dir name accum =
    let dirents = Sys.readdir ref_dir in
    Array.fold_left (fun accum dentry ->
      let new_name = String.concat "/" [name; dentry] in
      let new_path = Filename.concat ref_dir dentry in
      if Sys.is_directory new_path then (
        helper new_path new_name accum
      ) else if not (ends_with ".lock" dentry) then (
        (get_loose_ref new_name) :: accum
      ) else accum
    ) accum dirents in
  let ref_dir = Filename.concat (get_repo_dir ()) "refs" in
  helper ref_dir "refs" []

let all () : obj_ref list =
  List.append (get_loose_refs ()) (get_packed_refs ())
