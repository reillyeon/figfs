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

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type context = Fuse_bindings.__fuse_context
val get_context : unit -> context
type xattr_flags = AUTO | CREATE | REPLACE
type statfs = {
  f_bsize : int64;
  f_blocks : int64;
  f_bfree : int64;
  f_bavail : int64;
  f_files : int64;
  f_ffree : int64;
  f_namelen : int64;
}
type operations = {
  getattr : string -> Unix.LargeFile.stats;
  readlink : string -> string;
  readdir : string -> int -> string list;
  opendir : string -> Unix.open_flag list -> int option;
  releasedir : string -> Unix.open_flag list -> int -> unit;
  fsyncdir : string -> bool -> int -> unit; 
  mknod : string -> int -> unit;
  mkdir : string -> int -> unit;
  unlink : string -> unit;
  rmdir : string -> unit;
  symlink : string -> string -> unit;
  rename : string -> string -> unit;
  link : string -> string -> unit;
  chmod : string -> int -> unit;
  chown : string -> int -> int -> unit;
  truncate : string -> int64 -> unit;
  utime : string -> float -> float -> unit;
  fopen : string -> Unix.open_flag list -> int option;
  read : string -> buffer -> int64 -> int -> int;
  write : string -> buffer -> int64 -> int -> int;
  release : string -> Unix.open_flag list -> int -> unit;
  flush : string -> int -> unit;
  statfs : string -> statfs;
  fsync : string -> bool -> int -> unit;
  listxattr : string -> string list;
  getxattr : string -> string -> string;
  setxattr : string -> string -> string -> xattr_flags -> unit;
  removexattr : string -> string -> unit;
}
val op_names_of_operations : operations -> Fuse_bindings.fuse_operation_names
val default_operations : operations
val main : Fuse_bindings.str array -> operations -> unit
