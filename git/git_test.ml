open OUnit
open Object
open Sha1file

let sha1file_test (h:hash) (e:obj) () =
  let v = read_sha1_file h in
  assert_equal v e

let sha1file_tree_test (h:hash) (e:(int * string * hash) list) () =
  sha1file_test h (Tree (h, e)) ()

let sha1file_blob_test (h:hash) (e:string) () =
  sha1file_test h (Blob (h, e)) ()

let sha1file_tests = "sha1file tests" >::: [
  "test1" >:: sha1file_tree_test "9154b7b094ef16273b917fda004079a8842b5d97"
     [(0o100644, ".gitignore", "25d2f290e4894f26fbaa13b04ecdfeed20eabdb8");
      (0o100644, "OMakefile",  "4d319aac0b9d7bf7c28600b83519f2c124fe295b");
      (0o100644, "OMakeroot",  "35c219dacbab2809f577cc0ba9c10854e8f9fb1f");
      (0o040000, "fuse",       "218244fc513e9d405f7947a9de956b479cb168be");
      (0o040000, "git",        "1ed4b3daacad3febf7288009e613734035a8c6dc");
      (0o040000, "zlib",       "2fbd671a6fff80321a57a5749da197e40bb0d4ab")];
  "test2" >:: sha1file_blob_test "25d2f290e4894f26fbaa13b04ecdfeed20eabdb8"
     ("*~\n*.[ao]\n*.l[ao]\n\n# Files related to O'Caml projects.\n*.cm[ioxa]" ^
      "\n*.cmxa\n*.omc\n*.omakedb*\n*.run\n*.opt\n")
]

let all_tests = "all tests" >::: [
  sha1file_tests
]

let _ =
  Repository.set_repo_dir "..";
  run_test_tt all_tests
