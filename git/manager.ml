(* Object manager.
 * Copyright (C) 2008-2009 Reilly Grant
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

let find_object (h:hash) : obj =
  try Cache.find_object h
  with Not_found ->
    let stat, data =
      try Loosefile.find_object_raw h
      with Not_found -> Packfile.find_object_raw h in
    let objekt = parse_obj stat.os_hash stat.os_type data in
    Cache.add_object objekt;
    Cache.add_stat stat;
    objekt

let stat_object (h:hash) : obj_stat =
  try Cache.stat_object h
  with Not_found ->
    let stat =
      try Loosefile.stat_object h
      with Not_found -> Packfile.stat_object h in
    Cache.add_stat stat;
    stat

let traverse_tree (h:hash) (path:string) : obj =
  let rec helper h path =
    match path with
    | item :: rest -> (
      let objekt = find_object h in
      match objekt with
      | Commit c -> helper c.c_tree path
      | Tree t ->
        let (_,_,hash) =
          List.find (fun (_,name,_) -> name = item) t.t_dirents in
        helper hash rest
      | Blob b -> raise Not_found
      | Tag g -> helper g.g_object path
    )
    | [] -> (
      match find_object h with
      | Commit c -> helper c.c_tree path
      | o -> o
    )
  in helper h (Util.split path '/')
