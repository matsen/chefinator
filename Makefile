default: chefinator.native

%.native %.byte %.p.native:
	ocamlbuild $@

clean:
	ocamlbuild -clean

%.top:
	ocamlbuild $@

%.runtop: %.top
	ledit -x -h .toplevel_history ./$*.top

runcaml:
	ledit -x -h .toplevel_history ocaml

tags:
	taggage `find . -name "*.ml" | grep -v "_build"`

.PHONY: $(RELEASE) clean runcaml tags test docs
