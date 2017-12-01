
.PHONY: build clean test

build:
	jbuilder build @install --dev

test:
	jbuilder runtest --dev --no-buffer

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build
