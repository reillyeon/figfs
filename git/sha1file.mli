(* Git object database unpacked files. *)

open Object

val find_sha1_file : hash -> string

val read_sha1_file : hash -> obj
