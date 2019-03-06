
.PHONY: build clean test

build:
	dune build @install

test:
	dune runtest --no-buffer

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
