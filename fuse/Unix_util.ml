(*
  This file is part of the "OCamlFuse" library.

  OCamlFuse is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation (version 2 of the License).
  
  OCamlFuse is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with OCamlFuse.  See the file LICENSE.  If you haven't received
  a copy of the GNU General Public License, write to:
  
  Free Software Foundation, Inc.,
  59 Temple Place, Suite 330, Boston, MA
  02111-1307  USA

  Vincenzo Ciancia

  applejack@users.sf.net
  vincenzo_ml@yahoo.it
*)

open Result

external read_noexn : Unix.file_descr -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> int result = "unix_util_read"
external write_noexn : Unix.file_descr -> (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t -> int result = "unix_util_write"

external int_of_file_descr : Unix.file_descr -> int = "unix_util_int_of_file_descr"
external file_descr_of_int : int -> Unix.file_descr = "unix_util_file_descr_of_int"

let read fd buf =
  match read_noexn fd buf with
      Ok res -> res
    | Bad err -> raise (Unix.Unix_error (err,"read",""))

let write fd buf =
  match write_noexn fd buf with
      Ok res -> res
    | Bad err -> raise (Unix.Unix_error (err,"read",""))
