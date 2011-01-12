#
# aws-ocaml 0.2
#

all:
	ocamlbuild src/aws.cmo

clean:
	ocamlbuild -clean
	find . | grep '~' | xargs rm -rf 