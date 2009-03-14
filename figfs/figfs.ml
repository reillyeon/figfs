(* Filesystem Interface to Git.
 * Copyright (C) 2008 Reilly Grant
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
open Manager
open Object
open Refs
open Util

open Unix
open Unix.LargeFile

type open_file = {
    of_read : Fuse.buffer -> int64 -> int;
    of_write : Fuse.buffer -> int64 -> int;
  }

let open_files : (int, open_file) Hashtbl.t = Hashtbl.create 8

let next_fd : int ref = ref 0

let rec new_fd () : int =
  if Hashtbl.mem open_files !next_fd then (
    incr next_fd;
    new_fd ()
  ) else (
    let fd = !next_fd in
    incr next_fd;
    fd
  )

let print (msg:string) =
  Printf.printf "%s\n" msg;
  flush Pervasives.stdout

let base_stat = {
  st_dev = 0;
  st_ino = 0;
  st_kind = Unix.S_REG;
  st_perm = 0o644;
  st_nlink = 1;
  st_uid = Unix.getuid ();
  st_gid = Unix.getgid ();
  st_rdev = 0;
  st_size = 0L;
  st_atime = Unix.time ();
  st_mtime = Unix.time ();
  st_ctime = Unix.time ()
}

let commit_README = String.concat "\n" [
  "This directory appears empty, but directories within it exist if the given";
  "commit exists."; ""]

(* Splits a path of the form /xxx/root/path into (root, path). (Where path may
 * be the empty string. *)
let split_root_path (path:string) : string * string =
  let root_start = String.index_from path 1 '/' + 1 in
  try
    let root_end = String.index_from path root_start '/' in
    let root = String.sub path root_start (root_end - root_start) in
    let remaining_path = String.sub path root_end
        (String.length path - root_end) in
    root, remaining_path
  with Not_found ->
    String.sub path root_start (String.length path - root_start), "/"

let commit_getattr (path:string) : Unix.LargeFile.stats =
  let commit, remaining_path = split_root_path path in
  if remaining_path = "" || remaining_path = "/" then
    { base_stat with
      st_kind = Unix.S_DIR;
      st_perm = 0o755 }
  else try (
    let dir = traverse_tree commit (Filename.dirname remaining_path) in
    match dir with
    | Tree t ->
        let (perms,_,file_hash) = List.find (fun (_,n,_) -> n =
          (Filename.basename remaining_path)) t.t_dirents in
        let file_stat = stat_object file_hash in
        let kind =
          match file_stat.os_type with
          | TTree -> Unix.S_DIR
          | TBlob -> Unix.S_REG
          | _ -> failwith "Getattr of commit?" in
        { base_stat with
          st_kind = kind;
          st_perm = if file_stat.os_type = TBlob then perms else 0o755;
          st_size = Int64.of_int file_stat.os_size }
    | _ -> failwith "Parent object not directory?"
   ) with Not_found -> raise (Unix.Unix_error (Unix.ENOENT, "getattr", path))

let ref_getattr (path:string) (refbase:string) : Unix.LargeFile.stats =
  let name, rest = split_root_path path in
  try
    let ref = Refs.lookup (String.concat "/" [refbase; name]) in
    commit_getattr (String.concat "" ["/commit/"; ref.r_target; rest])
  with Not_found -> raise (Unix.Unix_error (Unix.ENOENT, "getattr", path))

let figfs_getattr (path:string) : Unix.LargeFile.stats =
  if starts_with "/commit/" path && String.length path >= 48 then
    commit_getattr path
  else if starts_with "/tag/" path && String.length path > 5 then
    ref_getattr path "refs/tags"
  else if starts_with "/branch/" path && String.length path > 8 then
    ref_getattr path "refs/heads"
  else if "/commit" = path || "/tag" = path || "/branch" = path ||
          "/" = path then
    { base_stat with
      st_kind = Unix.S_DIR;
      st_perm = 0o755 }
  else if "/commit/README" = path then
    { base_stat with
      st_size = Int64.of_int (String.length commit_README) }
  else
    raise (Unix.Unix_error (Unix.ENOENT, "getattr", path))

let commit_readdir (path:string) : string list =
  let commit, remaining_path = split_root_path path in
  let entries =
    try
      let dir = traverse_tree commit remaining_path in
      match dir with
      | Tree t -> List.map (fun (_,n,_) -> n) t.t_dirents
      | _ -> raise (Unix.Unix_error (Unix.ENOTDIR, "readdir", path))
    with Not_found -> raise (Unix.Unix_error (Unix.ENOENT, "readdir", path))
  in "." :: ".." :: entries

let ref_readdir (refbase:string) (path:string) : string list =
  let name, rest = split_root_path path in
  try
    let ref = Refs.lookup (String.concat "/" [refbase; name]) in
    commit_readdir (String.concat "" ["/commit/"; ref.r_target; rest])
  with Not_found -> raise (Unix.Unix_error (Unix.ENOENT, "readdir", path))

let refdir_readdir (refbase:string) : string list =
  let all_refs = Refs.all () in
  let all_names = List.map (fun r -> r.r_name) all_refs in
  let cat_refs = List.filter (fun s -> starts_with refbase s) all_names in
  let base_len = String.length refbase + 1 in
  let cat_names = List.map (fun s ->
    String.sub s base_len (String.length s - base_len)) cat_refs in
  "." :: ".." :: cat_names

let figfs_readdir (path:string) (fd:int) : string list =
  if starts_with "/commit/" path && String.length path >= 48 then
    commit_readdir path
  else if starts_with "/tag/" path && String.length path > 5 then
    ref_readdir "refs/tags" path
  else if starts_with "/branch/" path && String.length path > 8 then
    ref_readdir "refs/heads" path
  else if "/tag" = path then
    refdir_readdir "refs/tags"
  else if "/branch" = path then
    refdir_readdir "refs/heads"
  else if "/commit" = path then
    ["."; ".."; "README"]
  else if "/" = path then
    ["."; ".."; "commit"; "tag"; "branch"]
  else
    raise (Unix.Unix_error (Unix.ENOENT, "readdir", path))

(* Copies the contents of the given string into the given buffer, returning
 * the number of bytes copied. *)
let string_to_buffer_blit (str:string) (start:int) (len:int) (buf:Fuse.buffer)
    : int =
  for i = start to (start + len - 1) do
    Bigarray.Array1.set buf (i - start) (String.get str i)
  done;
  len

let static_reader (path:string) (data:string) : Fuse.buffer -> int64 -> int =
  fun (buf:Fuse.buffer) (off:int64) ->
    let offset = Int64.to_int off in
    if offset < 0 || offset >= (String.length data)
    then raise (Unix.Unix_error (Unix.EINVAL, "read", path))
    else
      let to_read =
        min (String.length data - offset) (Bigarray.Array1.dim buf) in
      string_to_buffer_blit data offset to_read buf

let static_writer (path:string) : Fuse.buffer -> int64 -> int =
  fun _ _ -> raise (Unix.Unix_error (Unix.EACCES, "write", path))

let static_fopen (path:string) (data:string) : int =
  let fd = new_fd () in
  let openfd = { of_read = static_reader path data;
                 of_write = static_writer path } in
  Hashtbl.add open_files fd openfd;
  fd

let commit_fopen (path:string) : int =
  let commit, remaining_path = split_root_path path in
  try
    let file = traverse_tree commit remaining_path in
    match file with
    | Blob b -> static_fopen path b.b_data
    | _ -> raise (Unix.Unix_error (Unix.EISDIR, "fopen", path))
  with Not_found -> raise (Unix.Unix_error (Unix.ENOENT, "fopen", path))

let ref_fopen (refbase:string) (path:string) : int =
  let name, rest = split_root_path path in
  try
    let ref = Refs.lookup (String.concat "/" [refbase; name]) in
    commit_fopen (String.concat "" ["/commit/"; ref.r_target; rest])
  with Not_found -> raise (Unix.Unix_error (Unix.ENOENT, "fopen", path))

let figfs_fopen (path:string) (flags:Unix.open_flag list) : int option =
  if starts_with "/commit/" path && String.length path >= 48 then
    Some (commit_fopen path)
  else if starts_with "/tag/" path && String.length path > 5 then
    Some (ref_fopen "refs/tags" path)
  else if starts_with "/branch/" path && String.length path > 5 then
    Some (ref_fopen "refs/heads" path)
  else if "/commit/README" = path then
    Some (static_fopen path commit_README)
  else
    raise (Unix.Unix_error (Unix.ENOENT, "fopen", path))

let figfs_release (path:string) (flags:Unix.open_flag list) (fd:int) : unit =
  Hashtbl.remove open_files fd

let figfs_read (path:string) (buf:Fuse.buffer) (off:int64) (fd:int) : int =
  let openfd = Hashtbl.find open_files fd in
  openfd.of_read buf off

let figfs_write (path:string) (buf:Fuse.buffer) (off:int64) (fd:int) : int =
  let openfd = Hashtbl.find open_files fd in
  openfd.of_write buf off

let commit_readlink (path:string) : string =
  let commit, remaining_path = split_root_path path in
  try
    let file = traverse_tree commit remaining_path in
    match file with
    | Blob b -> b.b_data
    | _ -> raise (Unix.Unix_error (Unix.EINVAL, "readlink", path))
  with Not_found -> raise (Unix.Unix_error (Unix.ENOENT, "readlink", path))

let ref_readlink (refbase:string) (path:string) : string =
  let name, rest = split_root_path path in
  try
    let ref = Refs.lookup (String.concat "/" [refbase; name]) in
    commit_readlink (String.concat "" ["/commit/"; ref.r_target; rest])
  with Not_found -> raise (Unix.Unix_error (Unix.ENOENT, "readlink", path))

let figfs_readlink (path:string) : string =
  if starts_with "/commit/" path && String.length path >= 48 then
    commit_readlink path
  else if starts_with "/tag/" path && String.length path > 5 then
    ref_readlink "refs/tags" path
  else if starts_with "/branch/" path && String.length path > 8 then
    ref_readlink "refs/heads" path
  else
    raise (Unix.Unix_error (Unix.ENOENT, "readlink", path))

let operations : Fuse.operations = {
  Fuse.default_operations with
  Fuse.getattr  = figfs_getattr;
  Fuse.readdir  = figfs_readdir;
  Fuse.fopen    = figfs_fopen;
  Fuse.release  = figfs_release;
  Fuse.read     = figfs_read;
  Fuse.write    = figfs_write;
  Fuse.readlink = figfs_readlink
}

let make_absolute path =
  let slash_index =
    try String.index path '/'
    with Not_found -> -1 in
  if slash_index <> 0
  then Printf.sprintf "%s/%s" (Sys.getcwd ()) path
  else path

let repo_dir = ref (find_repo ())

let fuse_opts = ref []

let argspec =
  [("-r", Arg.String (fun s -> repo_dir := Some (make_absolute s)),
    "repository path");
   ("-d", Arg.Unit (fun () -> fuse_opts := "-d" :: !fuse_opts),
    "debug output (implies -f)");
   ("-f", Arg.Unit (fun () -> fuse_opts := "-f" :: !fuse_opts),
    "foreground operation");
   ("-s", Arg.Unit (fun () -> fuse_opts := "-s" :: !fuse_opts),
    "disable-multithreaded operation");
   ("-o", Arg.String (fun s ->
     fuse_opts := Printf.sprintf "-o %s" s :: !fuse_opts),
    "Fuse options.")]

let argrest s =
  fuse_opts := s :: !fuse_opts

let argusage = "usage: figfs [options] mountpoint"

let check_figfs_dir () =
  let figfs_dir = Filename.concat (get_repo_dir ()) ".figfs" in
  if Sys.file_exists figfs_dir then (
    if Sys.is_directory figfs_dir then ((* good *))
    else failwith "Repository contains .figfs, but it isn't a directory."
  ) else mkdir figfs_dir 0o755

let _ =
  try (
    Arg.parse argspec argrest argusage;
    match !repo_dir with
    | Some dir -> (
        set_repo_dir dir;
        check_figfs_dir ();
        Workspace.init ();
        let args = Array.of_list ("figfs" :: List.rev !fuse_opts) in
        Fuse.main args operations
      )
    | None -> failwith "No repository found."
  ) with
  | Failure s -> Printf.eprintf "%s\n" s
  | Sys_error s -> Printf.eprintf "%s\n" s
