play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

clean:
	ocamlbuild -clean
	rm -f checktypes.ml
	rm -f a2src.zip
