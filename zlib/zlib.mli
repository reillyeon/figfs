
(* Use the zlib inflate algorithm to decompress n bytes from the given 
 * string.  If less than n decompressed bytes are available the contents
 * of the rest of the string are undefined. *)
val inflate : string -> int -> string
