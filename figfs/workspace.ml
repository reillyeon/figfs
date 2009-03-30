(* Figfs writable workspace management.
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

open Repository
open Util

module Mode = struct
  type t =
    | File
    | Directory of int
    | Whiteout
    | Unknown

  let to_string t : string =
    match t with
    | File -> "File"
    | Directory i -> Printf.sprintf "Directory %d" i
    | Whiteout -> "Whiteout"
    | Unknown -> "Unknown"
end

let dir () =
  String.concat "/" [get_repo_dir (); "figfs"]

let file_dir (workspace:string) =
  String.concat "" [(dir ()); "/"; workspace; ".ws"]

let file_path (workspace:string) (path:string) =
  String.concat "" [(file_dir workspace); path]

let dbHandle : Sqlite3.db option ref = ref None

let db () : Sqlite3.db =
  match !dbHandle with
  | Some h -> h
  | None ->
    let db = Sqlite3.db_open (Filename.concat (dir ()) "workspaces.db") in
    if Sqlite3.errcode db <> Sqlite3.Rc.OK
    then failwith (Sqlite3.errmsg db)
    else dbHandle := Some db; db

let prepareQuery (handle:Sqlite3.stmt option ref) (query:string) ()
    : Sqlite3.stmt =
  let db = db () in
  match !handle with
  | Some h -> h
  | None ->
    let query =
      try Sqlite3.prepare db query
      with Sqlite3.Error reason -> failwith (Printf.sprintf "sqlite: %s" reason)
    in handle := Some query; query

let listQueryHandle             : Sqlite3.stmt option ref = ref None
let addWorkspaceQueryHandle     : Sqlite3.stmt option ref = ref None
let existsWorkspaceQueryHandle  : Sqlite3.stmt option ref = ref None
let destroyWorkspaceQueryHandle : Sqlite3.stmt option ref = ref None
let clearWorkspaceQueryHandle   : Sqlite3.stmt option ref = ref None
let statFileQueryHandle         : Sqlite3.stmt option ref = ref None
let statWorkspaceQueryHandle    : Sqlite3.stmt option ref = ref None
let listDirQueryHandle          : Sqlite3.stmt option ref = ref None
let addFileQueryHandle          : Sqlite3.stmt option ref = ref None
let deleteFileQueryHandle       : Sqlite3.stmt option ref = ref None

let listQuery = prepareQuery listQueryHandle
    "SELECT name FROM workspace"
let addWorkspaceQuery = prepareQuery addWorkspaceQueryHandle
    "INSERT INTO workspace VALUES (?, ?)"
let existsWorkspaceQuery = prepareQuery existsWorkspaceQueryHandle
    "SELECT COUNT(*) FROM workspace WHERE name = ?"
let destroyWorkspaceQuery = prepareQuery destroyWorkspaceQueryHandle
    "DELETE FROM workspace WHERE name = ?"
let clearWorkspaceQuery = prepareQuery clearWorkspaceQueryHandle
    "DELETE FROM file WHERE workspace = ?"
let statFileQuery = prepareQuery statFileQueryHandle
    "SELECT rowid, directory, whiteout FROM file WHERE workspace = ? AND path = ?"
let statWorkspaceQuery = prepareQuery statWorkspaceQueryHandle
    "SELECT base FROM workspace WHERE name = ?"
let listDirQuery = prepareQuery listDirQueryHandle
    "SELECT name, whiteout FROM file WHERE parent = ?"
let addFileQuery = prepareQuery addFileQueryHandle
    "INSERT INTO file (workspace, path, name, parent, directory, whiteout) VALUES (?, ?, ?, ?, ?, ?)"
let deleteFileQuery = prepareQuery deleteFileQueryHandle
    "DELETE FROM file WHERE workspace = ? AND path = ?"

let init () : unit =
  let db = db () in
  if Sqlite3.exec db
      "CREATE TABLE IF NOT EXISTS workspace (name STRING PRIMARY KEY, base CHAR(40))"
      <> Sqlite3.Rc.OK
  then failwith (Printf.sprintf "sqlite: %s" (Sqlite3.errmsg db));
  if Sqlite3.exec db
      "CREATE TABLE IF NOT EXISTS file (id INTEGER PRIMARY KEY, workspace STRING, path STRING, name STRING, parent INTEGER, directory BOOLEAN, whiteout BOOLEAN)"
      <> Sqlite3.Rc.OK
  then failwith (Printf.sprintf "sqlite: %s" (Sqlite3.errmsg db))

let list () : string list =
  let stmt = listQuery () in
  if Sqlite3.reset stmt = Sqlite3.Rc.OK
  then (
    let rec loop accum =
      if Sqlite3.step stmt = Sqlite3.Rc.ROW
      then loop (Sqlite3.Data.to_string (Sqlite3.column stmt 0) :: accum)
      else accum
    in loop []
  ) else failwith (Printf.sprintf "sqlite: %s" (Sqlite3.errmsg (db ())))

let create (name:string) (hash:string) : unit =
  Unix.mkdir (file_dir name) 0o755;
  let stmt1 = addWorkspaceQuery () in
  if Sqlite3.reset stmt1 = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt1 1 (Sqlite3.Data.TEXT name) = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt1 2 (Sqlite3.Data.TEXT hash) = Sqlite3.Rc.OK &&
     Sqlite3.step stmt1 = Sqlite3.Rc.DONE then ()
  else failwith (Printf.sprintf "sqlite: %s" (Sqlite3.errmsg (db ())));
  let stmt2 = addFileQuery () in
  if Sqlite3.reset stmt2 = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt2 1 (Sqlite3.Data.TEXT name) = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt2 2 (Sqlite3.Data.TEXT "/") = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt2 3 (Sqlite3.Data.TEXT "") = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt2 4 (Sqlite3.Data.INT (-1L)) = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt2 5 (Sqlite3.Data.INT 1L) = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt2 6 (Sqlite3.Data.INT 0L) = Sqlite3.Rc.OK &&
     Sqlite3.step stmt2 = Sqlite3.Rc.DONE then ()
  else failwith (Printf.sprintf "sqlite: %s" (Sqlite3.errmsg (db ())))

let destroy (name:string) : unit =
  failwith "Workspace.destroy"

let stat_file (workspace:string) (path:string) : Mode.t =
  let stmt = statFileQuery () in
  if Sqlite3.reset stmt = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT workspace) = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT path) = Sqlite3.Rc.OK
  then (
    let rc = Sqlite3.step stmt in
    if rc = Sqlite3.Rc.ROW
    then (
      let id =
        match Sqlite3.column stmt 0 with
        | Sqlite3.Data.INT i -> Int64.to_int i
        | _ -> failwith "Expect int for file id." in
      let directory = Sqlite3.column stmt 1 in
      let whiteout = Sqlite3.column stmt 2 in
      if whiteout = (Sqlite3.Data.INT 1L) then Mode.Whiteout
      else if directory = (Sqlite3.Data.INT 1L) then (Mode.Directory id)
      else Mode.File
    ) else if rc = Sqlite3.Rc.DONE then Mode.Unknown
    else failwith (Printf.sprintf "sqlite: %s" (Sqlite3.errmsg (db ())))
  ) else failwith (Printf.sprintf "sqlite: %s" (Sqlite3.errmsg (db ())))

let list_dir (id:int) (base:string list): string list =
  let stmt = listDirQuery () in
  if Sqlite3.reset stmt = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)) = Sqlite3.Rc.OK
  then (
    let rec loop accum =
      if Sqlite3.step stmt = Sqlite3.Rc.ROW
      then (
        let name = Sqlite3.Data.to_string (Sqlite3.column stmt 0) in
        let whiteout = Sqlite3.column stmt 1 in
        if whiteout = (Sqlite3.Data.INT 1L)
        then loop (List.filter (fun n -> n <> name) accum)
        else loop (name :: (List.filter (fun n -> n <> name) accum))
      ) else accum
    in loop base
  ) else failwith (Printf.sprintf "sqlite: %s" (Sqlite3.errmsg (db ())))

let add_db_file (workspace:string) (parent:int) (path:string) (dir:bool)
    (whiteout:bool) : int =
  let parent = Int64.of_int parent in
  let dir = if dir then 1L else 0L in
  let whiteout = if whiteout then 1L else 0L in
  let name = Filename.basename path in
  let stmt = addFileQuery () in
  if Sqlite3.reset stmt = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT workspace) = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT path) = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT name) = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt 4 (Sqlite3.Data.INT parent) = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt 5 (Sqlite3.Data.INT dir) = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt 6 (Sqlite3.Data.INT whiteout) = Sqlite3.Rc.OK &&
     Sqlite3.step stmt = Sqlite3.Rc.DONE then ()
  else failwith (Printf.sprintf "sqlite: %s" (Sqlite3.errmsg (db ())));
  Int64.to_int (Sqlite3.last_insert_rowid (db ()))

let delete_db_file (workspace:string) (path:string) : unit =
  let stmt = deleteFileQuery () in
  if Sqlite3.reset stmt = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT workspace) = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT path) = Sqlite3.Rc.OK &&
     Sqlite3.step stmt = Sqlite3.Rc.DONE then ()
  else failwith (Printf.sprintf "sqlite: %s" (Sqlite3.errmsg (db ())))

let rec create_path (path:string) (names:string list) : unit =
    match names with
    | h1 :: h2 :: rest ->
        let new_path = Filename.concat path h1 in
        if not (Sys.file_exists new_path)
        then Unix.mkdir new_path 0o755;
        create_path new_path (h2 :: rest)
    | _ -> ()

let rec create_db_path (workspace:string) (parent:int) (path:string)
    (names:string list) : int =
    match names with
    | h1 :: h2 :: rest -> (
        let new_path = Filename.concat path h1 in
        match stat_file workspace new_path with
        | Mode.File -> failwith "Path component not a directory."
        | Mode.Directory id ->
            create_db_path workspace id new_path (h2 :: rest)
        | Mode.Whiteout ->
            delete_db_file workspace new_path;
            let new_id = add_db_file workspace parent new_path true false in
            create_db_path workspace new_id new_path (h2 :: rest)
        | Mode.Unknown ->
            let new_id = add_db_file workspace parent new_path true false in
            create_db_path workspace new_id new_path (h2 :: rest)
      )
    | _ -> parent

let create_file (workspace:string) (path:string) (data:string) (mode:int)
    : unit =
  let path_elems = split path '/' in
  create_path (file_dir workspace) path_elems;
  let wsfile = file_path workspace path in
  let fd = open_out wsfile in
  output_string fd data;
  close_out fd;
  let root =
    match stat_file workspace "/" with
    | Mode.Directory id -> id
    | _ -> failwith "Something is wrong, root directory isn't a directory." in
  let dir = create_db_path workspace root "/" path_elems in
  ignore (add_db_file (workspace:string) dir path false false)

let delete_file (workspace:string) (path:string) : unit =
  failwith "Workspace.delete_file"

let base (workspace:string) : string =
  let stmt = statWorkspaceQuery () in
  if Sqlite3.reset stmt = Sqlite3.Rc.OK &&
     Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT workspace) = Sqlite3.Rc.OK
  then (
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> Sqlite3.Data.to_string (Sqlite3.column stmt 0)
    | Sqlite3.Rc.DONE -> raise Not_found
    | _ -> failwith (Printf.sprintf "sqlite: %s" (Sqlite3.errmsg (db ())))
  ) else failwith (Printf.sprintf "sqlite: %s" (Sqlite3.errmsg (db ())))
