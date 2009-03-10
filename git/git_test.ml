(* Git library unit tests.
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

open OUnit
open Object
open Refs

let change_repo (dir:string) () : unit =
  Repository.set_repo_dir dir;
  Cache.clear ()

(* Test repository data. *)

let commit1 = Commit {
  c_hash = "57f9482bee1ff9e54dfd12024c041d3a8ab34ddf";
  c_tree = "296e56023cdc034d2735fee8c0d85a659d1b07f4";
  c_parents = [];
  c_author = "Reilly Grant <reillyeon@qotw.net> 1225584998 -0400";
  c_committer = "Reilly Grant <reillyeon@qotw.net> 1225584998 -0400";
  c_message = "Initial commit, two empty files.\n" }

let commit2 = Commit {
  c_hash = "96828b6633da42da034196d12af3fe4332b4b347";
  c_tree = "96b342b9881b0d31a6d182fcee0be26ac6187d92";
  c_parents = ["57f9482bee1ff9e54dfd12024c041d3a8ab34ddf"];
  c_author = "Reilly Grant <reillyeon@qotw.net> 1225585026 -0400";
  c_committer = "Reilly Grant <reillyeon@qotw.net> 1225585026 -0400";
  c_message = "Put data in the two files.\n" }

let commit3 = Commit {
  c_hash = "20bedc9d11bdbfdb473bbfa1c43a6c6f8fd06853";
  c_tree = "c5c94f726f68616a39a2f53686ae85c8755c3f4d";
  c_parents = ["96828b6633da42da034196d12af3fe4332b4b347"];
  c_author = "Reilly Grant <reillyeon@qotw.net> 1226381998 -0500";
  c_committer = "Reilly Grant <reillyeon@qotw.net> 1226381998 -0500";
  c_message = "Add a directory.\n" }

let tree1 = Tree {
  t_hash = "296e56023cdc034d2735fee8c0d85a659d1b07f4";
  t_dirents = [(0o100644, "a", "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391");
               (0o100644, "b", "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391")] }

let tree2 = Tree {
  t_hash = "96b342b9881b0d31a6d182fcee0be26ac6187d92";
  t_dirents = [(0o100644, "a", "c10c78b1d82190e1b339c6ca92a30438f3a3ba7d");
               (0o100644, "b", "d0c4445ef1c52236f14ed9a36a97a5404727240c")] }

let tree3 = Tree {
  t_hash = "c5c94f726f68616a39a2f53686ae85c8755c3f4d";
  t_dirents = [(0o100644, "a", "c10c78b1d82190e1b339c6ca92a30438f3a3ba7d");
               (0o100644, "b", "d0c4445ef1c52236f14ed9a36a97a5404727240c");
               (0o040000, "c", "2c1d0e7302ad98e1a6b3da8fcc60e84b40522b8f")] }

let tree4 = Tree {
  t_hash = "2c1d0e7302ad98e1a6b3da8fcc60e84b40522b8f";
  t_dirents = [(0o100644, "d", "257cc5642cb1a054f08cc83f2d943e56fd3ebe99");
               (0o100644, "e", "5716ca5987cbf97d6bb54920bea6adde242d87e6")] }

let blob1 = Blob {
  b_hash = "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391";
  b_data = "" }

let blob2 = Blob {
  b_hash = "c10c78b1d82190e1b339c6ca92a30438f3a3ba7d";
  b_data = "This is file a.\n" }

let blob3 = Blob {
  b_hash = "d0c4445ef1c52236f14ed9a36a97a5404727240c";
  b_data = "This is file b.\n" }

let blob4 = Blob {
  b_hash = "257cc5642cb1a054f08cc83f2d943e56fd3ebe99";
  b_data = "foo\n" }

let blob5 = Blob {
  b_hash = "5716ca5987cbf97d6bb54920bea6adde242d87e6";
  b_data = "bar\n" }

let tag0 = Tag {
  g_hash = "db7471e7aa23b42e59f4363bab8c00568b58ad5c";
  g_object = "96828b6633da42da034196d12af3fe4332b4b347";
  g_type = TCommit;
  g_name = "tag0";
  g_tagger = Some "Reilly Grant <reillyeon@qotw.net> 1231637882 -0500";
  g_message = "This is a tag.\n" }

let refs_loose =
  [{ r_name = "refs/heads/master";
     r_target = "20bedc9d11bdbfdb473bbfa1c43a6c6f8fd06853";
     r_peeled = None };
   { r_name = "refs/tags/tag0";
     r_target = "db7471e7aa23b42e59f4363bab8c00568b58ad5c";
     r_peeled = None }]

let refs_packed =
  [{ r_name = "refs/tags/tag0";
     r_target = "db7471e7aa23b42e59f4363bab8c00568b58ad5c";
     r_peeled = Some "96828b6633da42da034196d12af3fe4332b4b347" };
   { r_name = "refs/heads/master";
     r_target = "20bedc9d11bdbfdb473bbfa1c43a6c6f8fd06853";
     r_peeled = None }]

let refs_test (e:obj_ref list) () =
  let refs = Refs.all () in
  assert_equal refs e

let ref_test (ref:string) (e:obj_ref) () =
  let v = Refs.lookup ref in
  assert_equal v e

let refs_tests = "reference tests" >::: [
  "HEAD" >:: ref_test "HEAD"
              { r_name = "refs/heads/master";
                r_target = "20bedc9d11bdbfdb473bbfa1c43a6c6f8fd06853";
                r_peeled = None };
]

let object_test (e:obj) () =
  let v = Manager.find_object (hash_of_obj e) in
  if v <> e
  then Printf.printf "Expected: %s\nGot: %s\n" (string_of_obj e) (string_of_obj v)
  else ();
  assert_equal v e

let object_tests = "object tests" >::: [
  "commit1" >:: object_test commit1;
  "commit2" >:: object_test commit2;
  "commit3" >:: object_test commit3;
  "tree1" >:: object_test tree1;
  "tree2" >:: object_test tree2;
  "tree3" >:: object_test tree3;
  "tree4" >:: object_test tree4;
  "blob1" >:: object_test blob1;
  "blob2" >:: object_test blob2;
  "blob3" >:: object_test blob3;
  "blob4" >:: object_test blob4;
  "blob5" >:: object_test blob5;
  "tag0" >:: object_test tag0
]

(* Tree traversal tests. *)

let tree_traverse_test (c:hash) (p:string) (e:obj) () =
  let v = Manager.traverse_tree c p in
  assert_equal v e

let tree_traverse_fail_test (c:hash) (p:string) () =
  assert_raises Not_found (fun _ -> Manager.traverse_tree c p)

let tree_traverse_tests = "tree traversal tests" >::: [
  "commit1" >:: tree_traverse_test (hash_of_obj commit1) "/" tree1;
  "commit2" >:: tree_traverse_test (hash_of_obj commit2) "/" tree2;
  "tag0" >:: tree_traverse_test (hash_of_obj tag0) "/" tree2;
  "blob1a" >:: tree_traverse_test (hash_of_obj commit1) "/a" blob1;
  "blob1b" >:: tree_traverse_test (hash_of_obj commit1) "/b" blob1;
  "blob2" >:: tree_traverse_test (hash_of_obj commit2) "/a" blob2;
  "blob3" >:: tree_traverse_test (hash_of_obj commit2) "/b" blob3;
  "tree4" >:: tree_traverse_test (hash_of_obj commit3) "/c" tree4;
  "blob4" >:: tree_traverse_test (hash_of_obj commit3) "/c/d" blob4;
  "blob5" >:: tree_traverse_test (hash_of_obj commit3) "/c/e" blob5;
  "fail1" >:: tree_traverse_fail_test (hash_of_obj commit3) "/d";
  "fail2" >:: tree_traverse_fail_test (hash_of_obj commit3) "/c/d/f";
  "fail3" >:: tree_traverse_fail_test (hash_of_obj commit3) "/e/f"
]

(* Pack file tests. *)

let test_repo2_pack = "12f3aa44160a2407b01a744b9584cec9758ddec6"

let enumerate_packs_test () =
  let v = Packfile.enumerate_packs () in
  assert_equal v [test_repo2_pack]

let scan_index_test (h:hash) (expected:int64) () =
  let v = Packfile.scan_index test_repo2_pack h
  in assert_equal v expected

let scan_index_fail_test (h:hash) () =
  assert_raises Not_found (fun () -> Packfile.scan_index test_repo2_pack h)

let packfile_tests = "pack file tests" >::: [
  "set test_repo2" >:: change_repo "test_repo2";
  "enumerate1" >:: enumerate_packs_test;
  "scan1" >:: scan_index_test "20bedc9d11bdbfdb473bbfa1c43a6c6f8fd06853" 0xcL;
  "scan2" >:: scan_index_test "257cc5642cb1a054f08cc83f2d943e56fd3ebe99" 0x2abL;
  "scan3" >:: scan_index_test "296e56023cdc034d2735fee8c0d85a659d1b07f4" 0x2d4L;
  "scan4" >:: scan_index_test "2c1d0e7302ad98e1a6b3da8fcc60e84b40522b8f" 0x269L;
  "scan5" >:: scan_index_test "5716ca5987cbf97d6bb54920bea6adde242d87e6" 0x2b8L;
  "scan6" >:: scan_index_test "57f9482bee1ff9e54dfd12024c041d3a8ab34ddf" 0x150L;
  "scan7" >:: scan_index_test "96828b6633da42da034196d12af3fe4332b4b347" 0xaaL;
  "scan8" >:: scan_index_test "96b342b9881b0d31a6d182fcee0be26ac6187d92" 0x2c5L;
  "scan9" >:: scan_index_test "c10c78b1d82190e1b339c6ca92a30438f3a3ba7d" 0x239L;
  "scana" >:: scan_index_test "c5c94f726f68616a39a2f53686ae85c8755c3f4d" 0x1dbL;
  "scanb" >:: scan_index_test "d0c4445ef1c52236f14ed9a36a97a5404727240c" 0x251L;
  "scanc" >:: scan_index_test "db7471e7aa23b42e59f4363bab8c00568b58ad5c" 0x30bL;
  "scand" >:: scan_index_test "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391" 0x302L;
  "fail1" >:: scan_index_fail_test "0000000000000000000000000000000000000000";
  "fail2" >:: scan_index_fail_test "c10c78b1d82190e1b339c6ca92a30430f3a3ba7d";
  "fail3" >:: scan_index_fail_test "2c1d0e7302ad98e2a6b3da8fcc60e84b40522b8f";
  "fail4" >:: scan_index_fail_test "d4c4445ef1c52236f14ed9a36a97a5404727240c";
  "fail5" >:: scan_index_fail_test "ffffffffffffffffffffffffffffffffffffffff";
  "refs" >:: refs_test refs_packed;
  object_tests;
  refs_tests;
  tree_traverse_tests;
]

let loosefile_tests = "loose file test" >::: [
  "set test_repo1" >:: change_repo "test_repo1";
  "refs" >:: refs_test refs_loose;
  object_tests;
  refs_tests;
  tree_traverse_tests
]

let all_tests = "all tests" >::: [
  loosefile_tests;
  packfile_tests;
]

let _ =
  run_test_tt all_tests;

