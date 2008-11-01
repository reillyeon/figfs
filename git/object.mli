(* Object types and parser.
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

type hash = string

type obj_type =
  | TCommit
  | TTree
  | TBlob

val string_of_obj_type : obj_type -> string

val obj_type_of_string : string -> obj_type

type commit = {
    c_hash : hash; (* Hash of this commit. *)
    c_tree : hash; (* Tree for this commit. *)
    c_parents : hash list; (* Parents of this commit. *)
    c_author : string; (* Author of this commit. *)
    c_committer : string; (* Person who made this commit. *)
    c_message : string (* Commit message. *)
  }

val string_of_commit : commit -> string

type dirent = int * string * hash

val string_of_dirent : dirent -> string

type tree = {
    t_hash : hash; (* Hash of this tree. *)
    t_dirents : dirent list (* Entries under this tree. *)
  }

val string_of_tree : tree -> string

type blob = {
    b_hash : hash; (* Hash of this blob. *)
    b_data : string (* Data. *)
  }

val string_of_blob : blob -> string

type obj =
  | Commit of commit
  | Tree of tree
  | Blob of blob

val string_of_obj : obj -> string

val hash_of_obj : obj -> hash

val type_of_obj : obj -> obj_type

val read_obj : hash -> obj_type -> string -> obj
