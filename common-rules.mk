.SUFFIXES: .o .c .cmo .cmx .cmi .ml .mli

.c.o:
	gcc $(CFLAGS) -c $< -o $@

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<
