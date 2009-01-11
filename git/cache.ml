(* Data caching.
 * Copyright (C) 2008-2009 Reilly Grant
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

open Object

let object_cache : (hash, obj) Hashtbl.t = Hashtbl.create 8

let stat_cache : (hash, obj_stat) Hashtbl.t = Hashtbl.create 8

let find_object (h:hash) : obj =
  Hashtbl.find object_cache h

let add_object (o:obj) : unit =
  Hashtbl.add object_cache (hash_of_obj o) o

let stat_object (h:hash) : obj_stat =
  Hashtbl.find stat_cache h

let add_stat (os:obj_stat) : unit =
  Hashtbl.add stat_cache os.os_hash os

let clear () : unit =
  Hashtbl.clear object_cache;
  Hashtbl.clear stat_cache
