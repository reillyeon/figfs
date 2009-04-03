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

module Mode : sig
  type t =
    | File
    | Directory of int
    | Whiteout
    | Unknown

  val to_string : t -> string
end

val init : unit -> unit

val list : unit -> string list

val create : string -> string -> unit

val destroy : string -> unit

val file_path : string -> string -> string

val stat_file : string -> string -> Mode.t

val list_dir : int -> string list -> string list

val create_file : string -> string -> string -> int -> unit

val delete_file : string -> string -> unit

val create_dir : string -> string -> int -> unit

val delete_dir : string -> string -> unit

val base : string -> string
