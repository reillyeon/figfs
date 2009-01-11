(* Object types and parser.
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

type hash = string

type obj_type =
  | TCommit
  | TTree
  | TBlob
  | TTag
  | TOfsDelta
  | TRefDelta

let string_of_obj_type : obj_type -> string = function
  | TCommit -> "commit"
  | TTree -> "tree"
  | TBlob -> "blob"
  | TTag -> "tag"
  | TOfsDelta -> "offset delta"
  | TRefDelta -> "reference delta"

let obj_type_of_string : string -> obj_type = function
  | "commit" -> TCommit
  | "tree" -> TTree
  | "blob" -> TBlob
  | "tag" -> TTag
  | t -> failwith (Printf.sprintf "Unknown object type: %s" t)

let obj_type_of_int : int -> obj_type = function
  | 1 -> TCommit
  | 2 -> TTree
  | 3 -> TBlob
  | 4 -> TTag
  | 6 -> TOfsDelta
  | 7 -> TRefDelta
  | t -> failwith (Printf.sprintf "Unknown object type: %d" t)

type obj_stat = {
    os_hash : hash; (* Object hash. *)
    os_type : obj_type; (* Object type. *)
    os_size : int (* Object size. *)
  }

type commit = {
    c_hash : hash; (* Hash of this commit. *)
    c_tree : hash; (* Tree for this commit. *)
    c_parents : hash list; (* Parents of this commit. *)
    c_author : string; (* Author of this commit. *)
    c_committer : string; (* Person who made this commit. *)
    c_message : string (* Commit message. *)
  }

let string_of_commit commit : string =
  let string_of_parents parents =
    match parents with
    | parent :: [] -> "Parent: " ^ parent
    | _ -> let merges = List.map (fun p -> "Merge: " ^ p) parents
           in String.concat "\n" merges in
  Printf.sprintf
    "Commit: %s\nTree: %s\n%s\nAuthor: %s\nCommitter: %s\n\n%s"
    commit.c_hash commit.c_tree (string_of_parents commit.c_parents)
    commit.c_author commit.c_committer commit.c_message

type dirent = int * string * hash

let string_of_dirent ((p,n,h):dirent) : string =
  Printf.sprintf "%s %06o %s" h p n

type tree = {
    t_hash : hash; (* Hash of this tree. *)
    t_dirents : dirent list (* Entries under this tree. *)
  }

let string_of_tree tree : string =
  let entstrings = List.map (fun (p,n,h) -> Printf.sprintf "%s %06o %s" h p n)
                     tree.t_dirents
  in Printf.sprintf "Tree: %s\n%s" tree.t_hash (String.concat "\n" entstrings)

type blob = {
    b_hash : hash; (* Hash of this blob. *)
    b_data : string (* Data. *)
  }

let string_of_blob blob : string =
  Printf.sprintf "Blob %s {\n%s\n}" blob.b_hash blob.b_data

type tag = {
    g_hash : hash; (* Hash of this tag. *)
    g_object : hash; (* Object tagged. *)
    g_type : obj_type; (* Type of object tagged. *)
    g_name : string; (* Name of tag. *)
    g_tagger : string; (* Author of tag. *)
    g_message : string (* Tag message. *)
  }

let string_of_tag tag : string =
  Printf.sprintf "Tag %s : %s {\nObject: %s\nType: %s\nTagger: %s\n\n%s\n}"
    tag.g_name tag.g_hash tag.g_object (string_of_obj_type tag.g_type)
    tag.g_tagger tag.g_message

type obj =
  | Commit of commit
  | Tree of tree
  | Blob of blob
  | Tag of tag

let string_of_obj : obj -> string = function
  | Commit c -> string_of_commit c
  | Tree t -> string_of_tree t
  | Blob b -> string_of_blob b
  | Tag g -> string_of_tag g

let hash_of_obj : obj -> hash = function
  | Commit c -> c.c_hash
  | Tree t -> t.t_hash
  | Blob b -> b.b_hash
  | Tag g -> g.g_hash

let type_of_obj : obj -> obj_type = function
  | Commit _ -> TCommit
  | Tree _ -> TTree
  | Blob _ -> TBlob
  | Tag _ -> TTag

let parse_commit (data:string) : hash * hash list * string * string * string =
  let rec helper data tree_acc parent_acc author_acc committer_acc =
    let newline_index = String.index data '\n' in
    if newline_index = 0
    then
      let m = String.sub data 1 (String.length data - 1) in
      match (tree_acc, parent_acc, author_acc, committer_acc) with
      | (Some t, p, Some a, Some c) -> (t,p,a,c,m)
      | _ -> failwith "Commit parse not complete."
    else
      let space_index = String.index data ' ' in
      let field = String.sub data 0 space_index in
      let value = String.sub data (space_index + 1)
                                  (newline_index - space_index - 1) in
      let rest = String.sub data (newline_index + 1)
                                 (String.length data - newline_index - 1) in
      match field with
      | "tree" -> helper rest (Some value) parent_acc author_acc committer_acc
      | "parent" ->
          helper rest tree_acc (value :: parent_acc) author_acc committer_acc
      | "author" -> helper rest tree_acc parent_acc (Some value) committer_acc
      | "committer" -> helper rest tree_acc parent_acc author_acc (Some value)
      | _ -> failwith ("Unknown commit property: " ^ field)
  in helper data None [] None None

let parse_tag (data:string) : hash * obj_type * string * string * string =
    let rec helper data object_acc type_acc name_acc tagger_acc =
    let newline_index = String.index data '\n' in
    if newline_index = 0
    then
      let m = String.sub data 1 (String.length data - 1) in
      match object_acc, type_acc, name_acc, tagger_acc with
      | (Some o, Some t, Some n, Some a) -> (o,t,n,a,m)
      | _ -> failwith "Tag parse not complete."
    else
      let space_index = String.index data ' ' in
      let field = String.sub data 0 space_index in
      let value = String.sub data (space_index + 1)
                                  (newline_index - space_index - 1) in
      let rest = String.sub data (newline_index + 1)
                                 (String.length data - newline_index - 1) in
      match field with
      | "object" -> helper rest (Some value) type_acc name_acc tagger_acc
      | "type" ->helper rest object_acc (Some (obj_type_of_string value))
            name_acc tagger_acc
      | "tag" -> helper rest object_acc type_acc (Some value) tagger_acc
      | "tagger" -> helper rest object_acc type_acc name_acc (Some value)
      | _ -> failwith ("Unknown tag property: " ^ field)
  in helper data None None None None

let rec parse_tree (data:string) : (int * string * hash) list =
  let data_length = String.length data in
  if data_length = 0
  then []
  else
    let space_index = String.index data ' ' in
    let null_index = String.index data '\000' in
    let perms = int_of_string ("0o" ^ String.sub data 0 space_index) in
    let name = String.sub data (space_index + 1)
        (null_index - space_index - 1) in
    let hash = Hash.base16_of_base256 (String.sub data (null_index + 1) 20) in
    (perms, name, hash) :: parse_tree (String.sub data (null_index + 21)
                                         (data_length - null_index - 21))

let parse_obj (h:hash) (typ:obj_type) (data:string) : obj =
  match typ with
  | TCommit ->
      let tree, parents, author, committer, message = parse_commit data in
      Commit { c_hash = h;
               c_tree = tree;
               c_parents = parents;
               c_author = author;
               c_committer = committer;
               c_message = message }
  | TTree -> Tree { t_hash = h;
                    t_dirents = parse_tree data }
  | TBlob -> Blob { b_hash = h;
                    b_data = data }
  | TTag ->
      let objekt, typ, name, tagger, message = parse_tag data in
      Tag { g_hash = h;
            g_object = objekt;
            g_type = typ;
            g_name = name;
            g_tagger = tagger;
            g_message = message }
  | TOfsDelta -> failwith "Cannot parse delta object."
  | TRefDelta -> failwith "Cannot parse delta object."
