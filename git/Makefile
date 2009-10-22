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

all: $(if $(BYTE_ENABLED), git.cma cat-file git-test) \
     $(if $(NATIVE_ENABLED), git.cmxa cat-file.opt git-test.opt) \

OCAMLFLAGS = -package unix,oUnit,camlzip -warn-error -A -g -I .

OCAML_SOURCE = hash.ml \
               util.ml \
               object.ml \
               repository.ml \
               loosefile.ml \
               delta.ml \
               packfile.ml \
               cache.ml \
               manager.ml \
               refs.ml \

OCAML_INTERFACES = $(OCAML_SOURCE:.ml=.mli)
OCAML_BYTE_OBJECTS = $(OCAML_SOURCE:.ml=.cmo)
OCAML_BYTE_INTERFACES = $(OCAML_INTERFACES:.mli=.cmi)
OCAML_NATIVE_OBJECTS = $(OCAML_SOURCE:.ml=.cmx)
OCAML_CODE_OBJECTS = $(OCAML_SOURCE:.ml=.o)

git.cmxa git.a: $(OCAML_NATIVE_OBJECTS)
	$(OCAMLOPT) $(OCAMLFLAGS) -a -linkall -o $@ $^

git.cma: $(OCAML_BYTE_OBJECTS)
	$(OCAMLC) $(OCAMLFLAGS) -a -linkall -o $@ $^

cat-file: git.cma cat_file.cmo
	$(OCAMLC) $(OCAMLFLAGS) -custom -o $@ $^ -linkpkg

cat-file.opt: git.cmxa cat_file.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ $^ -linkpkg

git-test: git.cma git_test.cmo
	$(OCAMLC) $(OCAMLFLAGS) -custom -o $@ $^ -linkpkg

git-test.opt: git.cmxa git_test.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ $^ -linkpkg

.PHONY: test clean

test_repo1: test_repo1.tar.bz2
	tar xjf test_repo1.tar.bz2

test_repo2: test_repo2.tar.bz2
	tar xjf test_repo2.tar.bz2

test: git-test test_repo1 test_repo2
ifdef BYTE_ENABLED
	./git-test
endif
ifdef NATIVE_ENABLED
	./git-test.opt
endif

clean:
	rm .depend
	rm -rf test_repo1 test_repo2
	rm -f $(OCAML_BYTE_INTERFACES) cat_file.cmi git_test.cmi
ifdef BYTE_ENABLED
	rm -f cat-file cat_file.cmo
	rm -f git-test git_test.cmo
	rm -f git.cma
	rm -f $(OCAML_BYTE_OBJECTS)
endif
ifdef NATIVE_ENABLED
	rm -f cat-file.opt cat_file.cmx cat_file.o
	rm -f git-test.opt git_test.cmx git_test.o
	rm -f git.cmxa git.a
	rm -f $(OCAML_CODE_OBJECTS) $(OCAML_NATIVE_OBJECTS)
endif

.depend: $(OCAML_SOURCE) $(OCAML_INTERFACES)
	ocamldep $^ > .depend

include ../common-rules.mk
include .depend