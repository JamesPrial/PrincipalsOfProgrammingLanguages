ifeq ($(origin NETID), undefined)
NETID:=$(shell whoami)
endif

all: proj2_types.cmo proj2.cmo proj2_driver.cmo 

proj2.cmi: proj2.mli
	ocamlc -c proj2.mli

proj2_driver.cmi: proj2_driver.ml proj2.ml proj2_types.ml
	ocamlc -c proj2_driver.mli

proj2_driver.cmo: proj2_driver.cmi proj2_driver.ml proj2_types.ml
	ocamlc -c str.cma proj2_driver.ml

proj2.cmo: proj2.cmi proj2.ml
	ocamlc -c proj2.ml

proj2_types.cmi: proj2_types.mli
	ocamlc -c proj2_types.mli

proj2_types.cmo: proj2_types.cmi proj2_types.ml
	ocamlc -c proj2_types.ml

codetest.cmo: proj2_types.cmi codetest.ml
	ocamlc -c codetest.ml

submission: proj2_$(NETID).tar.gz

proj2_$(NETID).tar.gz:
	tar -cvzf proj2_$(NETID).tar.gz --transform 's,^,proj2/,' proj2.ml

clean:
	rm -f *.cmo *.cmi *.cmx *.o proj2_*.tar.gz 

test: proj2_types.cmo  proj2.cmo proj2_driver.cmo  codetest.ml
	ocaml -I ./ proj2_types.cmo  proj2.cmo str.cma proj2_driver.cmo codetest.ml

.PHONY: all clean submission test
