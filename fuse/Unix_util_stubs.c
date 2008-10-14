/*
  This file is part of the "OCamlFuse" library.

  OCamlFuse is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation (version 2 of the License).
  
  OCamlFuse is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with OCamlFuse.  See the file LICENSE.  If you haven't received
  a copy of the GNU General Public License, write to:
  
  Free Software Foundation, Inc.,
  59 Temple Place, Suite 330, Boston, MA
  02111-1307  USA

  Vincenzo Ciancia

  applejack@users.sf.net
  vincenzo_ml@yahoo.it
*/

#include <stddef.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#ifdef Custom_tag
#include <caml/custom.h>
#include <caml/bigarray.h>
#endif
#include <caml/camlidlruntime.h>

int c2ml_unix_error(int c_err);

value unix_util_read(value fd,value buf)
{
  value vres=alloc(1,1); /* Ok result */
  int res;
  enter_blocking_section();
  res = read(Int_val(fd), /* TODO: unsafe coercion */
	     Bigarray_val(buf)->data,Bigarray_val(buf)->dim[0]); 
  leave_blocking_section();
  if (res >=0) Field(vres,0)=Val_int(res);
  else 
    {
      Tag_val(vres)=0; /* Bad result */
      Field(vres,0)=Val_int(c2ml_unix_error(res)); /* TODO: EUNKNOWN x is a block */
    }
  return vres;
}

value unix_util_write(value fd,value buf)
{
  value vres=alloc(1,1); /* Ok result */
  int res;
  enter_blocking_section();
  res = write(Int_val(fd), /* TODO: unsafe coercion */
	      Bigarray_val(buf)->data,Bigarray_val(buf)->dim[0]);
  leave_blocking_section();
  if (res >=0) Field(vres,0)=Val_int(res);
  else 
    {
      Tag_val(vres)=0; /* Bad result */
      Field(vres,0)=Val_int(c2ml_unix_error(res)); /* TODO: EUNKNOWN x is a block */
    }
  return vres;
}

value unix_util_int_of_file_descr(value fd)
{
  return Val_int(Int_val(fd) /* TODO: unsafe coercion */ );
}

value unix_util_file_descr_of_int(value fd)
{
  return Val_int(Int_val(fd) /* TODO: unsafe coercion */ );
}
