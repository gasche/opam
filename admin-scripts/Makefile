%: %.ml
	sed 's/^#.*//' $< >$*_tmp.ml
	ocamlfind ocamlc -package opam-lib,opam-lib.repository -linkpkg ../src/tools/opam_admin_top.ml $*_tmp.ml -o $@
	rm $*_tmp.{ml,cmi,cmo}

couverture: couverture.ml
	sed 's/^#.*//' $< >couverture_tmp.ml
	ocamlfind ocamlopt -package re.glob,opam-lib.state -linkpkg ../src/tools/opam_admin_top.ml couverture_tmp.ml -o $@
	rm couverture_tmp.{ml,cmi,cmx,o}
