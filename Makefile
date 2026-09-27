.PHONY: build
build:
	dune build @install @examples

.PHONY: examples
examples:
	dune build @examples

.PHONY: doc
doc:dune build @doc -p zmq-async,zmq-eio,zmq-lwt,zmq
	dune build @doc

.PHONY: test
test:
	dune runtest --force

.PHONY: install
install:
	dune install

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: publish
publish:
	dune-release -p zmq-async,zmq-eio,zmq-lwt,zmq

.PHONY: clean
clean:
	dune clean

gh-pages:
	dune clean
	dune build @doc -p zmq-async,zmq-eio,zmq-lwt,zmq
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp  -r _build/default/_doc/_html/* .gh-pages
	git -C .gh-pages add .
	git -C .gh-pages config user.email 'docs@ocaml-zmq'
	git -C .gh-pages commit -m "Update documentation"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages
