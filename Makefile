default: miniLCF

miniLCF:
	ocamlbuild miniLCF.native

clean:
	ocamlbuild -clean
