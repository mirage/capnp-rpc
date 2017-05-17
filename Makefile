all:
	jbuilder build --dev @install test/test.exe
	./_build/default/test/test.exe -ev

clean:
	rm -rf _build
