let _ =
  Repository.set_repo_dir "..";
  Workspace.init ();
  print_endline (String.concat " " (Workspace.list ()));
  print_endline (Workspace.Mode.to_string (Workspace.stat_file "test" "/foobar"));
  print_endline (Workspace.file_path "test" "/foobar");
  print_endline (Workspace.base "test");
