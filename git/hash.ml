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
  let n = i land 0xf in
  if n >= 0 && n < 10
  then char_of_int (n + 48)
  else char_of_int (n + 87)

let int_of_hex (c:char) : int =
  if '0' <= c && c <= '9'
  then int_of_char c - 48
  else if 'a' <= c && c <= 'f'
  then int_of_char c - 87
  else if 'A' <= c && c <= 'F'
  then int_of_char c - 55
  else failwith "invalid hexadecimal character"

let base256_of_base16 (s:string) : string =
  let length = String.length s / 2 in
  let buf = String.create (length) in
  for i = 0 to length - 1 do
    let a = int_of_hex (String.get s (i * 2)) in
    let b = int_of_hex (String.get s (i * 2 + 1)) in
    String.set buf i (char_of_int ((a lsl 4) lor b))
  done;
  buf

let base16_of_base256 (s:string) : string =
  let buf = String.create (String.length s * 2) in
  let offset = ref 0 in
  String.iter (fun c ->
    let n = int_of_char c in
    String.set buf !offset (hex_of_int (n lsr 4));
    String.set buf (!offset + 1) (hex_of_int n);
    offset := !offset + 2) s;
  buf
