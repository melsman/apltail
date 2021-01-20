#MLKIT=SML_LIB=/Users/mael/gits/mlkit /Users/mael/gits/mlkit/bin/mlkit

#MLCOMP ?= mlkit

MLCOMP ?= mlton
SMLPKG ?= smlpkg

FILES=src/flags.sml src/flags.mlb src/aplt.sml src/aplt.mlb \
  src/apl2tail.mlb src/Apl2Tail.sml src/Tail2Laila.sml \
  src/util.sig src/util.sml src/util.mlb \
  $(shell ls -1 src/tail/*.sig src/tail/*.sml src/tail/*.mlb) \
  $(shell ls -1 src/il/*.sig src/il/*.sml src/il/*.mlb) \
  $(shell ls -1 src/laila/*.sig src/laila/*.sml src/laila/*.mlb)

PREFIX ?= .
DESTDIR ?= $(PREFIX)/dist

GIT_VERSION := $(shell git --no-pager describe --tags --always --dirty)
GIT_DATE := $(firstword $(shell git --no-pager show --date=short --format="%ad" --name-only))
PLATFORM := $(shell uname -pmrs)

.PHONY: all
all: aplt

# Use temp file src/version~ to ensure regeneration of src/version.sml
# whenever (and only when) the git version changes...
.PHONY: force
src/version~: force
	@echo '$(GIT_VERSION) $(GIT_DATE)' | cmp -s - $@ || echo '$(GIT_VERSION) $(GIT_DATE)' > $@

src/version.sml: src/version~
	@echo "structure Version = struct\n\
   val version = \"$(GIT_VERSION)\"\n\
   val date = \"$(GIT_DATE)\"\n\
   val platform = \"$(PLATFORM)\"\nend" > $@
	@echo Generated file $@
	@echo Git version $(GIT_VERSION) $(GIT_DATE)

aplt: src/aplt.mlb $(FILES) src/aplt.sml src/version.sml src/lib/github.com/diku-dk/sml-aplparse
	$(MLCOMP) -output $@ $<

.PHONY: install
install:
	cp -p aplt $(DESTDIR)/bin/

OS=$(shell uname -s | tr '[:upper:]' '[:lower:]')

DISTNAME=apltail-bin-dist-$(OS)

.PHONY: dist
dist: aplt
	rm -rf dist
	mkdir dist
	mkdir dist/$(DISTNAME)
	mkdir dist/$(DISTNAME)/bin
	mkdir -p dist/$(DISTNAME)/lib/apltail
	mkdir -p dist/$(DISTNAME)/include/apltail
	mkdir -p dist/$(DISTNAME)/share/apltail/tests
	mkdir -p dist/$(DISTNAME)/share/apltail/doc
	cp -p aplt dist/$(DISTNAME)/bin/
	cp -p lib/prelude.apl dist/$(DISTNAME)/lib/apltail/
	cp -p include/apl.h dist/$(DISTNAME)/include/apltail/
	cp -p tests/Makefile tests/*.out.ok tests/*.apl tests/*.txt dist/$(DISTNAME)/share/apltail/tests/
	cp -p MIT_LICENSE.md dist/$(DISTNAME)/share/apltail/doc/MIT_LICENSE
	cp -p doc/README_BIN dist/$(DISTNAME)/share/apltail/doc/README
	echo 'PREFIX?=/usr/local' > dist/$(DISTNAME)/Makefile
	echo '.PHONY: install' >> dist/$(DISTNAME)/Makefile
	echo 'install:' >> dist/$(DISTNAME)/Makefile
	echo "\t"'for d in $$$$(find * -type d); do install -d "$$(PREFIX)/$$$$d"; done' \
                >> dist/$(DISTNAME)/Makefile
	echo "\t"'for f in $$$$(find * -type f | grep -v Makefile); do install -p "$$$$f" "$$(PREFIX)/$$$$f"; done' \
                >> dist/$(DISTNAME)/Makefile
	(cd dist; tar -czf $(DISTNAME).tgz $(DISTNAME))

.PHONY: test
test: aplt Makefile
	$(MAKE) -C tests test testc

.PHONY: prepare
prepare:
	(cd src; $(SMLPKG) sync)

.PHONY: clean
clean: Makefile
	find . -name '*~' | xargs rm -f
	find . -name 'MLB' | xargs rm -rf
	find . -name 'run' | xargs rm -f
	rm -f aplt src/version.sml
	$(MAKE) -C tests clean

src/lib/github.com/diku-dk/sml-aplparse:
	(cd src; $(SMLPKG) sync)
