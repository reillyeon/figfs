(* Git Objects *)

type hash = string

type obj_type =
  | TCommit
  | TTree
  | TBlob

val string_of_obj_type : obj_type -> string

val obj_type_of_string : string -> obj_type

type commit = {
    c_hash : hash; (* Hash of this commit. *)
    c_tree : hash; (* Tree for this commit. *)
    c_parents : hash list; (* Parents of this commit. *)
    c_author : string; (* Author of this commit. *)
    c_committer : string; (* Person who made this commit. *)
    c_date : string; (* Commit time. *)
    c_message : string (* Commit message. *)
  }

val string_of_commit : commit -> string

type dirent = int * string * hash

val string_of_dirent : dirent -> string

type tree = {
    t_hash : hash; (* Hash of this tree. *)
    t_dirents : dirent list (* Entries under this tree. *)
  }

val string_of_tree : tree -> string

type blob = {
    b_hash : hash; (* Hash of this blob. *)
    b_data : string (* Data. *)
  }

val string_of_blob : blob -> string

type obj =
  | Commit of commit
  | Tree of tree
  | Blob of blob

val string_of_obj : obj -> string

val hash_of_obj : obj -> hash

val type_of_obj : obj -> obj_type

val read_obj : hash -> obj_type -> string -> obj
