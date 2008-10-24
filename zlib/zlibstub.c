#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <zlib.h>

/**
 * Call inflate() to decompress n bytes from the given string.  If
 * less than n decompressed bytes are available the contents of the
 * rest of the string are undefined.
 */
CAMLprim value
caml_inflate(value input, value limit)
{
    char *in_buf, *out_buf;
    size_t in_buf_len, out_buf_len;
    value output;
    z_stream stream;
    CAMLparam2(input, limit);

    in_buf = String_val(input);
    in_buf_len = string_length(input);

    out_buf_len = Int_val(limit);
    if (out_buf_len <= 0) {
        caml_failwith("Inflate limit <= 0.");
    }
    output = caml_alloc_string(out_buf_len);
    out_buf = &Byte(output, 0);

    memset(&stream, 0, sizeof stream);
    stream.next_in = (unsigned char *)in_buf;
    stream.avail_in = in_buf_len;
    stream.next_out = (unsigned char *)out_buf;
    stream.avail_out = out_buf_len;

    inflateInit(&stream);
    if (stream.msg != NULL) {
        caml_failwith(stream.msg);
    }

    inflate(&stream, Z_FINISH);
    if (stream.msg != NULL) {
        caml_failwith(stream.msg);
    }

    inflateEnd(&stream);
    if (stream.msg != NULL) {
        caml_failwith(stream.msg);
    }

    CAMLreturn(output);
}
