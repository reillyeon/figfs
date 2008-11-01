(* Hash utilities.
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

let hex_of_int (i:int) : char =
  let n = i mod 16 in
  if n >= 0 && n < 10
  then char_of_int (n + 48)
  else char_of_int (n + 87)

let base256_of_base16 (s:string) : string =
  failwith "base256_of_base16"

let base16_of_base256 (s:string) : string =
  let length = String.length s in
  let buf = String.create (length * 2) in
  let offset = ref 0 in
  String.iter (fun c ->
    let n = int_of_char c in
    String.set buf !offset (hex_of_int (n lsr 4));
    String.set buf (!offset + 1) (hex_of_int n);
    offset := !offset + 2) s;
  buf
