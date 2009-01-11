(* Git pack file access.
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

(* Get directory where packs are stored. *)
val pack_directory : unit -> string

(* Get list of pack files (by hash). *)
val enumerate_packs : unit -> string list

(* Path to the index file for the given pack. *)
val index_path : string -> string

(* Path to the pack file for the given pack. *)
val pack_path : string -> string

(* Scan the given pack index for an object.  Result is the offset in the pack
 * file. *)
val scan_index : string -> Object.hash -> int64

val find_object_raw : Object.hash -> Object.obj_stat * string

val find_object : Object.hash -> Object.obj

val stat_object : Object.hash -> Object.obj_stat
