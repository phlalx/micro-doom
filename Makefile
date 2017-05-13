
main2d:
		ocamlbuild -pp "camlp4o.opt -unsafe" -libs graphics main2d.native	       
main3d:
		ocamlbuild -pp "camlp4o.opt -unsafe" -libs graphics main3d.native	       
clean:
		ocamlbuild -clean
		rm -f *~

