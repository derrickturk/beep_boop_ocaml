stats: textstat.mli textstat.ml stats.ml
	ocamlc -o $@ $^

stats.opt: textstat.mli textstat.ml stats.ml
	ocamlopt -o $@ $^

reverse: reverse.ml
	ocamlc -o $@ $^

reverse.opt: reverse.ml
	ocamlopt -o $@ $^

ogrep: ogrep.ml
	ocamlc -o $@ str.cma $^

ogrep.opt: ogrep.ml
	ocamlopt -o $@ str.cmxa $^
