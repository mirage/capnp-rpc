.PHONY: all fuzz clean test

default: test build-fuzz

all:
	jbuilder build --dev @install test/test.bc test-lwt/test.bc test-bin/calc.exe
	rm -rf _build/_tests
	jbuilder runtest --dev --no-buffer -j 1

build-fuzz:
	jbuilder build --dev fuzz/fuzz.exe

fuzz: build-fuzz
	# TODO: remove -d
	afl-fuzz -d -i _build/in -o _build/out -- ./_build/default/fuzz/fuzz.exe --fuzz

clean:
	rm -rf _build

test:
	rm -rf _build/_tests
	jbuilder build --dev test/test.bc test-lwt/test.bc test-bin/calc.bc
	#./_build/default/test/test.bc test core -ev 16
	jbuilder build @runtest --dev --no-buffer -j 1
