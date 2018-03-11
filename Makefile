.PHONY: all fuzz clean test

default: test build-fuzz

all:
	jbuilder build --dev @install test/test.bc test-lwt/test.bc test-bin/calc.exe test-mirage/test.bc
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
	jbuilder build --dev test/test.bc test-lwt/test.bc test-bin/calc.bc test-mirage/test.bc
	#./_build/default/test/test.bc test core -ev 12
	#./_build/default/test-lwt/test.bc test lwt -ev 3
	jbuilder build @runtest --dev --no-buffer -j 1

REPO=../opam-repository
PACKAGES=$(REPO)/packages
# until we have https://github.com/ocaml/opam-publish/issues/38
pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	cd $(PACKAGES) && git add $*

PKGS=$(basename $(wildcard *.opam))
opam-pkg:
	$(MAKE) $(PKGS:%=pkg-%)
