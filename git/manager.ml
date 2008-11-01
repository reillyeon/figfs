open Repository
open Object
open Sha1file

let find_object (h:hash) : obj =
  let sha1file_path = find_sha1_file h in
  if Sys.file_exists sha1file_path
  then read_sha1_file h
  else raise Not_found
