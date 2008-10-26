(* Git Objects *)

type hash = string

type obj_type =
  | TCommit
  | TTree
  | TBlob

type obj =
  | Commit of hash * string
  | Tree of hash * (int * string * hash) list
  | Blob of hash * string

val string_of_obj : obj -> string

val read_obj : hash -> obj_type -> string -> obj

val string_of_obj_type : obj_type -> string

val obj_type_of_string : string -> obj_type
