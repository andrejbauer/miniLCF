default: miniLCF

miniLCF:
	ocamlbuild miniLCF.native

debug:
	ocamlbuild miniLCF.d.byte

clean:
	ocamlbuild -clean
