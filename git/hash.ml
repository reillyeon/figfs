
let hex_of_int (i:int) : char =
  let n = i mod 16 in
  if n >= 0 && n < 10
  then char_of_int (n + 48)
  else char_of_int (n + 87)

let base256_of_base16 (s:string) : string =
  failwith "base256_of_base16"

let base16_of_base256 (s:string) : string =
  let length = String.length s in
  let buf = String.create (length * 2) in
  let offset = ref 0 in
  String.iter (fun c ->
    let n = int_of_char c in
    String.set buf !offset (hex_of_int (n lsr 4));
    String.set buf (!offset + 1) (hex_of_int n);
    offset := !offset + 2) s;
  buf
