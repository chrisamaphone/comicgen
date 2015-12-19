# the resulting working website is to be found in _build/html
all:
	ocamlbuild -use-ocamlfind \
	  -tag debug \
	  -plugin-tag "package(js_of_ocaml.ocamlbuild)" \
	  -package js_of_ocaml.ppx \
	  -package ppx_deriving_yojson \
	  -no-links \
	  html/index.html html/bind_gen.js

clean:
	ocamlbuild -clean
