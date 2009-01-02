(* Git delta format processor.
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

open Util

let patch (base:string) (diff:string) : string =
  let is_seven_bit c = int_of_char c land 0x80 = 0 in
  let out_size_pos = index_with diff is_seven_bit + 1 in
  let base_size = varint_to_int (String.sub diff 0 out_size_pos) in
  if base_size <> (String.length base)
  then failwith "Delta header disagrees with base size." else ();
  let diff_begin = index_from_with diff out_size_pos is_seven_bit + 1 in
  let out_size = varint_to_int (String.sub diff out_size_pos diff_begin) in
  let out = String.create out_size in
  let diff_pos = ref diff_begin in
  let out_pos = ref 0 in
  let size = ref out_size in
  while !diff_pos < (String.length diff) do
    let cmd = int_of_char (String.get diff !diff_pos) in
    incr diff_pos;
    if cmd land 0x80 <> 0 then (
      let cp_size = ref 0 in
      let cp_off = ref 0 in
      if cmd land 0x01 <> 0 then (
        cp_off := !cp_off lor (int_of_char (String.get diff !diff_pos));
        incr diff_pos
      );
      if cmd land 0x02 <> 0 then (
        cp_off := !cp_off lor (int_of_char (String.get diff !diff_pos) lsl 8);
        incr diff_pos
      );
      if cmd land 0x04 <> 0 then (
        cp_off := !cp_off lor (int_of_char (String.get diff !diff_pos) lsl 16);
        incr diff_pos
      );
      if cmd land 0x08 <> 0 then (
        cp_off := !cp_off lor (int_of_char (String.get diff !diff_pos) lsl 24);
        incr diff_pos
      );
      if cmd land 0x10 <> 0 then (
        cp_size := !cp_size lor (int_of_char (String.get diff !diff_pos));
        incr diff_pos
      );
      if cmd land 0x20 <> 0 then (
        cp_size := !cp_size lor (int_of_char (String.get diff !diff_pos) lsl 8);
        incr diff_pos
      );
      if cmd land 0x40 <> 0 then (
        cp_size := !cp_size lor (int_of_char (String.get diff !diff_pos) lsl 16);
        incr diff_pos
      );
      if !cp_off > 0 || !cp_off + !cp_size > base_size
      then failwith "Source copy command out of bounds."
      else (
        String.blit base !cp_off out !out_pos !cp_size;
        out_pos := !out_pos + !cp_size;
        size := !size - !cp_size
      )
    ) else if cmd <> 0 then (
      if cmd > !size
      then failwith "Delta copy command out of bounds."
      else (
        String.blit diff !diff_pos out !out_pos cmd;
        out_pos := !out_pos + cmd;
        diff_pos := !diff_pos + cmd;
        size := !size - cmd
      )
    ) else failwith "Invalid delta opcode: 0"
  done;
  if !size != 0
  then failwith "Ran out of delta data."
  else out
