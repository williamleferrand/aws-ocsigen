#
# aws-ocaml 0.2
#

all:
	ocamlbuild src/aws.cmo

clean:
	ocamlbuild -clean
	find . | grep '~' | xargs rm -rf 

install:
	ocamlfind install aws META _build/src/aws.cm*

remove:
	ocamlfind remove aws