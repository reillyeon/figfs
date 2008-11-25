(* Loose object file reader.
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

let find_loose_file (hash:hash) : string =
  let prefix = String.sub hash 0 2 in
  let suffix = String.sub hash 2 (String.length hash - 2) in
  List.fold_left Filename.concat (get_repo_dir ()) ["objects"; prefix; suffix]

let internal_read_loose_file (hash:hash) : obj_stat * string =
  let path = find_loose_file hash in
  let file = open_in_bin path in
  let file_size = in_channel_length file in
  let buf = String.create file_size in
  really_input file buf 0 file_size;
  let inflated = Zlib.inflate buf 50 in
  let space_index = String.index inflated ' ' in
  let null_index = String.index inflated '\000' in
  let typ = obj_type_of_string (String.sub inflated 0 space_index) in
  let size = int_of_string (String.sub inflated (space_index + 1) 
                              (null_index - space_index - 1)) in
  let inflated_data = Zlib.inflate buf (size + null_index + 1) in
  let data = String.sub inflated_data (null_index + 1) size in
  ( { os_hash = hash; os_type = typ; os_size = size }, data )

let stat_loose_file (hash:hash) : obj_stat =
  let stat, _ = internal_read_loose_file hash in
  stat

let read_loose_file (hash:hash) : obj =
  let stat, data = internal_read_loose_file hash in
  read_obj hash stat.os_type data
