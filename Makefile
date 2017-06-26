.PHONY: all fuzz clean test

default: test

all:
	jbuilder build --dev @install test/test.bc test-lwt/test.bc
	rm -rf _build/_tests
	jbuilder runtest --dev --no-buffer -j 1

build-fuzz:
	jbuilder build --dev fuzz/fuzz.exe

fuzz: build-fuzz
	# TODO: remove -d
	afl-fuzz -d -i _build/in -o _build/out ./_build/default/fuzz/fuzz.exe

clean:
	rm -rf _build

test:
	rm -rf _build/_tests
	jbuilder build --dev test/test.bc test-lwt/test.bc
	#./_build/default/test/test.bc test core -ev 0
	jbuilder build @runtest --dev --no-buffer -j 1
