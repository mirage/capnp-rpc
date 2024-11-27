.PHONY: all fuzz clean test

default: test build-fuzz

build-fuzz:
	dune build fuzz/fuzz.exe

fuzz: build-fuzz
	# TODO: remove -d
	afl-fuzz -d -i _build/in -o _build/out -- ./_build/default/fuzz/fuzz.exe --fuzz

clean:
	rm -rf _build

test:
	rm -rf _build/_tests
	dune build test test-bin @install
	dune build @runtest --no-buffer -j 1
