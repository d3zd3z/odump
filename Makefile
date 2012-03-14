# Simple build

#SETUP = setup.ml
#RUNSETUP = ocaml setup.ml

KIND = native

TARGETS = test.$(KIND) odump.$(KIND)
all: .force
	ocamlbuild $(TARGETS)

test: .force
	ocamlbuild $(TARGETS)
	./_build/test.$(KIND)

clean: .force
	ocamlbuild -clean

.PHONY: .force
