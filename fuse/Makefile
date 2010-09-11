# Copyright (C) 2009 Reilly Grant
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

include ../common-defs.mk

all: $(if $(BYTE_ENABLED), fuse.cma) \
     $(if $(NATIVE_ENABLED), fuse.cmxa)

CFLAGS = -fPIC -D_FILE_OFFSET_BITS=64 -pthread -I. -DPIC -DNATIVE_CODE -g -I/opt/local/lib/ocaml/ -D__FreeBSD__=10
OCAMLFLAGS = -thread -package unix,threads,bigarray -warn-error -A -g -I .

GENERATED_FILES = Fuse_bindings_stubs.c \
                  Fuse_bindings.ml \
                  Fuse_bindings.mli \
                  Fuse_bindings.h

C_SOURCE = Fuse_bindings_stubs.c \
           Fuse_util.c \
           Unix_util_stubs.c

C_OBJECTS = $(C_SOURCE:.c=.o)

OCAML_SOURCE = Fuse_bindings.ml \
               Result.ml \
               Fuse_lib.ml \
               Fuse.ml \
               Unix_util.ml

OCAML_INTERFACES = Fuse_bindings.mli \
                   Fuse.mli

OCAML_BYTE_OBJECTS = $(OCAML_SOURCE:.ml=.cmo)
OCAML_BYTE_INTERFACES = $(OCAML_INTERFACES:.mli=.cmi)
OCAML_NATIVE_OBJECTS = $(OCAML_SOURCE:.ml=.cmx)
OCAML_CODE_OBJECTS = $(OCAML_SOURCE:.ml=.o)

$(GENERATED_FILES): Fuse_bindings.idl
	camlidl -header $^

libfusestub.a: $(C_OBJECTS)
	ar rcs $@ $^

dllfuse.so: $(C_OBJECTS)
	gcc -o $@ -shared -Wl,-undefined,dynamic_lookup $(C_OBJECTS) -lfuse -lfuse -pthread  -liconv

fuse.cmxa fuse.a: $(OCAML_NATIVE_OBJECTS) libfusestub.a
	$(OCAMLOPT) $(OCAMLFLAGS) -a -linkall -cclib -lfusestub -cclib -lfuse  -cclib -lcamlidl -o $@ $(OCAML_NATIVE_OBJECTS)

fuse.cma: $(OCAML_BYTE_OBJECTS) libfusestub.a dllfuse.so
	$(OCAMLC) $(OCAMLFLAGS) -custom -a -dllib dllfuse.so -linkall bigarray.cma -cclib -lfusestub -cclib -lfuse -cclib -lcamlidl -o $@ $(OCAML_BYTE_OBJECTS)


.PHONY: clean

clean:
	rm .depend
	rm -f $(GENERATED_FILES)
	rm -f $(OCAML_BYTE_INTERFACES)
	rm -f Fuse_lib.cmi  Result.cmi    Unix_util.cmi
ifdef BYTE_ENABLED
	rm -f fuse.cma libfusestub.a dllfuse.so
	rm -f $(OCAML_BYTE_OBJECTS)
endif
ifdef NATIVE_ENABLED
	rm -f fuse.cmxa fuse.a libfusestub.a dllfuse.so
	rm -f $(OCAML_CODE_OBJECTS) $(OCAML_NATIVE_OBJECTS)
endif
	rm -f $(C_OBJECTS)


.depend: $(OCAML_SOURCE) $(OCAML_INTERFACES)
	ocamldep $^ > .depend

include ../common-rules.mk
include .depend
