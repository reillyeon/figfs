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

all: $(if $(BYTE_ENABLED), mount.figfs figfs figfs_test) \
     $(if $(NATIVE_ENABLED), mount.figfs.opt figfs.opt figfs_test.opt)

OCAMLFLAGS = -thread -package unix,threads,bigarray,camlzip,sqlite3 -warn-error -A -g -I ../git -I ../fuse -I .

OCAML_SOURCE = command.ml workspace.ml figfs.ml

OCAML_INTERFACES = $(OCAML_SOURCE:.ml=.mli)
OCAML_BYTE_OBJECTS = $(OCAML_SOURCE:.ml=.cmo)
OCAML_BYTE_INTERFACES = $(OCAML_INTERFACES:.mli=.cmi)
OCAML_NATIVE_OBJECTS = $(OCAML_SOURCE:.ml=.cmx)
OCAML_CODE_OBJECTS = $(OCAML_SOURCE:.ml=.o)

mount.figfs: $(OCAML_BYTE_OBJECTS) figfs_mount.cmo
	$(OCAMLC) $(OCAMLFLAGS) git.cma fuse.cma -custom -linkpkg -o $@ $^

mount.figfs.opt: $(OCAML_NATIVE_OBJECTS) figfs_mount.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) git.cmxa fuse.cmxa -linkpkg -o $@ $^

figfs: $(OCAML_BYTE_OBJECTS) figfs_tool.cmo
	$(OCAMLC) $(OCAMLFLAGS) git.cma fuse.cma -custom -linkpkg -o $@ $^

figfs.opt: $(OCAML_NATIVE_OBJECTS) figfs_tool.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) git.cmxa fuse.cmxa -linkpkg -o $@ $^

figfs_test: $(OCAML_BYTE_OBJECTS) figfs_test.cmo
	$(OCAMLC) $(OCAMLFLAGS) git.cma fuse.cma -custom -linkpkg -o $@ $^

figfs_test.opt: $(OCAML_NATIVE_OBJECTS) figfs_test.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) git.cmxa fuse.cmxa -linkpkg -o $@ $^

.PHONY: test clean

test:
ifdef BYTE_ENABLED
	./figfs_test
endif
ifdef NATIVE_ENABLED
	./figfs_test.opt
endif

clean:
	rm -f .depend
	rm -f $(OCAML_BYTE_INTERFACES)
	rm -f figfs_mount.cmi figfs_tool.cmi figfs_test.cmi
ifdef BYTE_ENABLED
	rm -f mount.figfs figfs_mount.cmo
	rm -f figfs figfs_tool.cmo
	rm -f figfs_test figfs_test.cmo
	rm -f $(OCAML_BYTE_OBJECTS)
endif
ifdef NATIVE_ENABLED
	rm -f mount.figfs.opt figfs_mount.cmx figfs_mount.o
	rm -f figfs.opt figfs_tool.cmx figfs_tool.o
	rm -f figfs_test.opt figfs_test.cmx figfs_test.o
	rm -f $(OCAML_CODE_OBJECTS) $(OCAML_NATIVE_OBJECTS)
endif

.depend: figfs_mount.ml figfs_tool.ml figfs_test.ml \
         $(OCAML_SOURCE) $(OCAML_INTERFACES)
	ocamldep $^ > .depend

include ../common-rules.mk
include .depend
