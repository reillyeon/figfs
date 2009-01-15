(* Utility functions.
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

(* Split the given string at the given character. *)
val split : string -> char -> string list

(* Split the given string at spaces. *)
val unwords : string -> string list

(* Convert the given variable-length encoded integer to a real int. *)
val int_of_varint : string -> int

(* Convert the given variable-length encoded integer (done in the offset style)
 * to a real int. *)
val int_of_offsetint : string -> int

(* Standard function composition. *)
val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

(* Find the index of the first character which satisfies the given
 * predicate. *)
val index_with : string -> (char -> bool) -> int

(* Find the index of the first character after the given position which
 * satisfies the given predicate. *)
val index_from_with : string -> int -> (char -> bool) -> int

(* Check if the second string starts with the first string. *)
val starts_with : string -> string -> bool

(* Check if the second string ends with the first string. *)
val ends_with : string -> string -> bool

(* Convert 4-byte string buffer (big-endian) to an int (only 31-bits). *)
val decode_int : string -> int

(* Convert 4-byte string buffer (big-endian) to an int32. *)
val decode_int32 : string -> int32

(* Convert 8-byte string buffer (big-endian) to an int64. *)
val decode_int64 : string -> int64

val is_seven_bit : char -> bool

(* Decompress the data available from the given file descriptor.  Continues
 * until zlib detects the end of the stream. *)
val inflate_file : Unix.file_descr -> string
