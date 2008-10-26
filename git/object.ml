type hash = string

type obj_type =
  | TCommit
  | TTree
  | TBlob

type obj =
  | Commit of hash * string
  | Tree of hash * (int * string * hash) list
  | Blob of hash * string

let string_of_obj_type : obj_type -> string = function
  | TCommit -> "commit"
  | TTree -> "tree"
  | TBlob -> "blob"

let obj_type_of_string : string -> obj_type = function
  | "commit" -> TCommit
  | "tree" -> TTree
  | "blob" -> TBlob
  | t -> failwith (Printf.sprintf "Unknown object type '%s'." t)

type dirent = int * string * hash

let string_of_dirent ((p,n,h):dirent) : string =
  Printf.sprintf "%s %06o %s" h p n

let string_of_obj : obj -> string = function
  | Commit (h, d) -> Printf.sprintf "Commit[%s] {\n%s\n}" h d
  | Tree (h, dirents) ->
      let entstrings = List.map string_of_dirent dirents in
      Printf.sprintf "Tree[%s] {\n%s\n}" h (String.concat "\n" entstrings)
  | Blob (h, d) -> Printf.sprintf "Blob[%s] {\n%s\n}" h d

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
  | TCommit -> Commit (h, data)
  | TTree -> Tree (h, parse_tree data)
  | TBlob -> Blob (h, data)
