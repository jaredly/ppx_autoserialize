build:
	jbuilder build @install

run: build
	jbuilder exec ppx_magic

test:
	jbuilder runtest

pin:
	opam pin add .

repin: build
	opam upgrade ppx_magic

build-all:
	jbuilder build --workspace jbuild-workspace.dev @install

test-all:
	jbuilder build --workspace jbuild-workspace.dev @runtest

.PHONY: build test pin repin build-all test-all