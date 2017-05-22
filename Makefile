.PHONY: all fuzz clean test

default: test

all:
	jbuilder build --dev @install
	rm -rf _build/_tests
	jbuilder runtest --dev # --no-buffer

fuzz:
	jbuilder build --dev fuzz/fuzz.exe
	#./_build/default/fuzz/fuzz.exe
	# TODO: remove -d
	afl-fuzz -d -i _build/in -o _build/out ./_build/default/fuzz/fuzz.exe @@

clean:
	rm -rf _build

test:
	rm -rf _build/_tests
	jbuilder build @runtest --dev # --no-buffer
