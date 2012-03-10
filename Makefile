# Simple build

all: setup.data force
	ocaml setup.ml -build

setup.data: setup.ml
	ocaml setup.ml -configure

setup.ml: _oasis
	oasis setup

distclean: clean force
	rm -f setup.data setup.log setup.ml _tags myocamlbuild.ml
clean: setup.data force
	ocaml setup.ml -clean

.PHONY: force
