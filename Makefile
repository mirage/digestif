.PHONY: all clean test

all:
	dune build

test:
	dune runtest

clean:
	dune clean
