(* References.
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

type obj_ref = {
    r_name : string;               (* ref name *)
    r_target : Object.hash;        (* object referenced *)
    r_peeled : Object.hash option; (* commit referenced by the tag *)
  }

val string_of_obj_ref : obj_ref -> string

val lookup : string -> obj_ref

val all : unit -> obj_ref list
