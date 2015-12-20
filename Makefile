#
# Makefile for ocaml lab 6.
#

# Default c
default: bs

#
# Variables.
#

OCAMLLEX  = ocamllex
OCAMLYACC = ocamlyacc
OPT       =

ifdef OPT
OCAMLC    = ocamlopt.opt
OEXT      = cmx
else
OCAMLC    = ocamlc
OEXT      = cmo
endif

INTERFACES = \
    sexpr.cmi           \
    parser.cmi          \
    ast.cmi             \
    env.cmi             \
    primitives.cmi      \
    eval.cmi

OBJS = \
    sexpr.$(OEXT)           \
    parser.$(OEXT)          \
    lexer.$(OEXT)           \
    ast.$(OEXT)             \
    env.$(OEXT)             \
    primitives.$(OEXT)      \
    eval.$(OEXT)


#
# Patterns.
#

%.ml: %.mll
	$(OCAMLLEX) $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $<

%.$(OEXT): %.ml %.cmi
	$(OCAMLC) -c $<

%.cmi: %.mli
	$(OCAMLC) -c $<


#
# Compilation targets.
#

lexer.$(OEXT): lexer.ml
	$(OCAMLC) -c $<

main.$(OEXT): main.ml
	$(OCAMLC) -c $<

bs: $(INTERFACES) $(OBJS) main.$(OEXT)
	$(OCAMLC) $(OBJS) main.$(OEXT) -o $@


#
# Tests.
#

test: bs
	./run_test


#
# Cleanup.
#

clean:
	rm -f *.cmi *.cmo *.cmx *.o lexer.ml parser.ml bs

