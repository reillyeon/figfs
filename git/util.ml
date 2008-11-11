(* Utility functions.
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
