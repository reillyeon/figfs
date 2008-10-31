type hash = string

type obj_type =
  | TCommit
  | TTree
  | TBlob

let string_of_obj_type : obj_type -> string = function
  | TCommit -> "commit"
  | TTree -> "tree"
  | TBlob -> "blob"

let obj_type_of_string : string -> obj_type = function
  | "commit" -> TCommit
  | "tree" -> TTree
  | "blob" -> TBlob
  | t -> failwith (Printf.sprintf "Unknown object type '%s'." t)

type commit = {
    c_hash : hash; (* Hash of this commit. *)
    c_tree : hash; (* Tree for this commit. *)
    c_parents : hash list; (* Parents of this commit. *)
    c_author : string; (* Author of this commit. *)
    c_committer : string; (* Person who made this commit. *)
    c_date : string; (* Commit time. *)
    c_message : string (* Commit message. *)
  }

let string_of_commit commit : string =
  let string_of_parents parents =
    match parents with
    | parent :: [] -> "Parent: " ^ parent
    | _ -> let merges = List.map (fun p -> "Merge: " ^ p) parents
           in String.concat "\n" merges in
  Printf.sprintf
    "Commit: %s\nTree: %s\n%s\nAuthor: %s\nCommitter: %s\nDate: %s\n\n%s"
    commit.c_hash commit.c_tree (string_of_parents commit.c_parents)
    commit.c_author commit.c_committer commit.c_date commit.c_message

type dirent = int * string * hash

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

type obj =
  | Commit of commit
  | Tree of tree
  | Blob of blob

let string_of_dirent ((p,n,h):dirent) : string =
  Printf.sprintf "%s %06o %s" h p n

let string_of_obj : obj -> string = function
  | Commit c -> string_of_commit c
  | Tree t -> string_of_tree t
  | Blob b -> string_of_blob b

let hash_of_obj : obj -> hash = function
  | Commit c -> c.c_hash
  | Tree t -> t.t_hash
  | Blob b -> b.b_hash

let type_of_obj : obj -> obj_type = function
  | Commit _ -> TCommit
  | Tree _ -> TTree
  | Blob _ -> TBlob

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

let read_obj (h:hash) (typ:obj_type) (data:string) : obj =
  match typ with
  | TCommit -> Commit { c_hash = h;
                        c_tree = "";
                        c_parents = [];
                        c_author = "";
                        c_committer = "";
                        c_date = "";
                        c_message = data }
  | TTree -> Tree { t_hash = h;
                    t_dirents = parse_tree data }
  | TBlob -> Blob { b_hash = h;
                    b_data = data }
