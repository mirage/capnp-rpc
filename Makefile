.PHONY: all fuzz clean test

default: test

all:
	jbuilder build --dev @install test/test.bc test-lwt/test.bc
	rm -rf _build/_tests
	jbuilder runtest --dev --no-buffer -j 1

fuzz:
	jbuilder build --dev fuzz/fuzz.exe
	#./_build/default/fuzz/fuzz.exe
	# TODO: remove -d
	afl-fuzz -d -i _build/in -o _build/out ./_build/default/fuzz/fuzz.exe @@

clean:
	rm -rf _build

test:
	rm -rf _build/_tests
	jbuilder build --dev test/test.bc test-lwt/test.bc
	jbuilder build @runtest --dev --no-buffer -j 1
