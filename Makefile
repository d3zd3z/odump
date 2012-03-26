# Simple build

#SETUP = setup.ml
#RUNSETUP = ocaml setup.ml

KIND = native

# Build typerex info.
# Comment out to build without
# export OCAMLFIND_CONF := $(PWD)/etc/findlib-typerex.conf

TARGETS = test.$(KIND) odump.$(KIND)
all: .force
	ocamlbuild $(TARGETS)

test: .force
	ocamlbuild $(TARGETS)
	./_build/test.$(KIND)

clean: .force
	ocamlbuild -clean

.PHONY: .force
