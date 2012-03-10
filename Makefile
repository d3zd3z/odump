# Simple build

#SETUP = setup.ml
#RUNSETUP = ocaml setup.ml

SETUP = _build/setup.byte
RUNSETUP = ./_build/setup.byte

all: setup.data $(SETUP) force
	$(RUNSETUP) -build

test: all
	$(RUNSETUP) -test

# This has to be run before we are able to compile setup.ml using ocamlbuild.
setup.data: setup.ml
	ocaml setup.ml -configure > setup.state

setup.ml: _oasis
	oasis setup

_build/setup.byte: setup.ml setup.data
	ocamlbuild setup.byte

distclean: clean force
	rm -f setup.data setup.log setup.ml _tags
	rm -f myocamlbuild.ml setup.state
clean: setup.data force
	ocaml setup.ml -clean

.PHONY: force
