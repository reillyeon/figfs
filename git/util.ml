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

let split (s:string) (d:char) : string list =
  let acc = ref [] in
  let start = ref 0 in
  let len = ref 0 in
  String.iter (fun c ->
    if c = d
    then (
      if !len != 0 then acc := String.sub s !start !len :: !acc else ();
      start := !start + !len + 1;
      len := 0
     )
    else (
      len := !len + 1
     )
  ) s;
  let result =
    if !len <> 0
    then String.sub s !start !len :: !acc
    else !acc
  in List.rev result

let unwords (s:string) : string list =
  split s ' '

let varint_to_int (s:string) : int =
  let rec loop pos off acc =
    if pos >= (String.length s)
    then failwith "Invalid variable-precision integer."
    else (
      let c = int_of_char (String.get s pos) in
      let r = acc lor ((c land 0x7f) lsl off) in
      if c land 0x80 <> 0
      then loop (pos + 1) (off + 7) r
      else r
    )
  in loop 0 0 0

let compose (f:'b -> 'c) (g:'a -> 'b) (v:'a) =
  f (g v)

let index_from_with (s:string) (pos:int) (f:char -> bool) : int =
  let rec loop pos =
    if pos >= (String.length s)
    then raise Not_found
    else (
      if f (String.get s pos)
      then pos
      else loop (pos + 1)
    )
  in loop pos

let index_with (s:string) (f:char -> bool) : int =
  index_from_with s 0 f

let starts_with (pat:string) (s:string) : bool =
  let rec loop pos =
    if pos >= (String.length pat) or (String.get s pos) <> (String.get pat pos)
    then false
    else if pos = (String.length pat - 1)
    then true
    else loop (pos + 1) in
  if (String.length s) < (String.length pat)
  then false
  else loop 0

let ends_with (pat:string) (s:string) : bool =
  let rec loop patpos spos =
    if patpos >= (String.length pat) or
      (String.get s spos) <> (String.get pat patpos)
    then false
    else if patpos = (String.length pat - 1)
    then true
    else loop (patpos + 1) (spos + 1) in
  if (String.length s) < (String.length pat)
  then false
  else loop 0 (String.length s - (String.length pat))

let decode_int (buf:string) : int =
  let a = int_of_char (String.get buf 0) lsl 24 in
  let b = int_of_char (String.get buf 1) lsl 16 in
  let c = int_of_char (String.get buf 2) lsl 8 in
  let d = int_of_char (String.get buf 3) in
  a lor b lor c lor d

let decode_int32 (buf:string) : int32 =
  let a = Int32.shift_left (Int32.of_int (int_of_char (String.get buf 0))) 24 in
  let b = Int32.shift_left (Int32.of_int (int_of_char (String.get buf 1))) 16 in
  let c = Int32.shift_left (Int32.of_int (int_of_char (String.get buf 2))) 8 in
  let d = Int32.of_int (int_of_char (String.get buf 3)) in
  Int32.logor (Int32.logor (Int32.logor a b) c) d

let decode_int64 (buf:string) : int64 =
  let a = Int64.shift_left (Int64.of_int (int_of_char (String.get buf 0))) 56 in
  let b = Int64.shift_left (Int64.of_int (int_of_char (String.get buf 1))) 48 in
  let c = Int64.shift_left (Int64.of_int (int_of_char (String.get buf 2))) 40 in
  let d = Int64.shift_left (Int64.of_int (int_of_char (String.get buf 3))) 32 in
  let e = Int64.shift_left (Int64.of_int (int_of_char (String.get buf 4))) 24 in
  let f = Int64.shift_left (Int64.of_int (int_of_char (String.get buf 5))) 16 in
  let g = Int64.shift_left (Int64.of_int (int_of_char (String.get buf 6))) 8 in
  let h = Int64.of_int (int_of_char (String.get buf 7)) in
  let abcd = Int64.logor (Int64.logor (Int64.logor a b) c) d in
  let efgh = Int64.logor (Int64.logor (Int64.logor e f) g) h in
  Int64.logor abcd efgh
