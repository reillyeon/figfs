
(* Bind to the C stub in zlibstub.c. *)
external inflate : string -> int -> string = "caml_inflate"
