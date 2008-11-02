(* Git library unit tests.
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

open OUnit
open Object
open Sha1file

let sha1file_test (h:hash) (e:obj) () =
  Repository.set_repo_dir "test_repo1";
  let v = read_sha1_file h in
  assert_equal v e

let sha1file_commit_test (h:hash) (e:commit) () =
  sha1file_test h (Commit { e with c_hash = h }) ()

let sha1file_tree_test (h:hash) (e:dirent list) () =
  sha1file_test h (Tree { t_hash = h; t_dirents = e }) ()

let sha1file_blob_test (h:hash) (e:string) () =
  sha1file_test h (Blob { b_hash = h; b_data = e }) ()

let sha1file_tests = "sha1file tests" >::: [
  "commit1" >:: sha1file_commit_test "57f9482bee1ff9e54dfd12024c041d3a8ab34ddf"
     { c_hash = ""; c_tree = "296e56023cdc034d2735fee8c0d85a659d1b07f4";
       c_parents = [];
       c_author = "Reilly Grant <reillyeon@qotw.net> 1225584998 -0400";
       c_committer = "Reilly Grant <reillyeon@qotw.net> 1225584998 -0400";
       c_message = "Initial commit, two empty files.\n" };
  "commit2" >:: sha1file_commit_test "96828b6633da42da034196d12af3fe4332b4b347"
     { c_hash = ""; c_tree = "96b342b9881b0d31a6d182fcee0be26ac6187d92";
       c_parents = ["57f9482bee1ff9e54dfd12024c041d3a8ab34ddf"];
       c_author = "Reilly Grant <reillyeon@qotw.net> 1225585026 -0400";
       c_committer = "Reilly Grant <reillyeon@qotw.net> 1225585026 -0400";
       c_message = "Put data in the two files.\n" };
  "tree1" >:: sha1file_tree_test "296e56023cdc034d2735fee8c0d85a659d1b07f4"
     [(0o100644, "a", "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391");
      (0o100644, "b", "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391")];
  "tree2" >:: sha1file_tree_test "96b342b9881b0d31a6d182fcee0be26ac6187d92"
     [(0o100644, "a", "c10c78b1d82190e1b339c6ca92a30438f3a3ba7d");
      (0o100644, "b", "d0c4445ef1c52236f14ed9a36a97a5404727240c")];
  "blob1" >:: sha1file_blob_test "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"
     "";
  "blob2" >:: sha1file_blob_test "c10c78b1d82190e1b339c6ca92a30438f3a3ba7d"
     "This is file a.\n";
  "blob3" >:: sha1file_blob_test "d0c4445ef1c52236f14ed9a36a97a5404727240c"
     "This is file b.\n"
]

let all_tests = "all tests" >::: [
  sha1file_tests
]

let _ =
  run_test_tt all_tests
