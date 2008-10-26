open Repository
open Object

let find_sha1_file (hash:hash) : string =
  let prefix = String.sub hash 0 2 in
  let suffix = String.sub hash 2 (String.length hash - 2) in
  get_repo_dir () ^ "/.git/objects/" ^ prefix ^ "/" ^ suffix

let read_sha1_file (hash:hash) : obj =
  let path = find_sha1_file hash in
  let file = open_in_bin path in
  let file_size = in_channel_length file in
  let buf = String.create file_size in
  really_input file buf 0 file_size;
  let inflated = Zlib.inflate buf 50 in
  let space_index = String.index inflated ' ' in
  let null_index = String.index inflated '\000' in
  let typ = obj_type_of_string (String.sub inflated 0 space_index) in
  let size = int_of_string (String.sub inflated (space_index + 1) 
                              (null_index - space_index - 1)) in
  let inflated_data = Zlib.inflate buf (size + null_index + 1) in
  let data = String.sub inflated_data (null_index + 1) size in
  read_obj hash typ data
