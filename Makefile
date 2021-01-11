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

aplt: src/aplt.mlb $(FILES) src/aplt.sml src/version.sml
	$(MLCOMP) -output $@ $<

.PHONY: install
install:
	cp -p aplt $(DESTDIR)/bin/

DISTPOSTFIX?=linux
DISTNAME=apltail-$(DISTPOSTFIX)

.PHONY: dist
dist:
	rm -rf dist
	mkdir dist
	mkdir dist/$(DISTNAME)
	mkdir dist/$(DISTNAME)/bin
	mkdir dist/$(DISTNAME)/lib
	mkdir dist/$(DISTNAME)/include
	mkdir dist/$(DISTNAME)/tests
	mkdir dist/$(DISTNAME)/doc
	cp -p aplt dist/$(DISTNAME)/bin/
	cp -p lib/prelude.apl dist/$(DISTNAME)/lib/
	cp -p include/apl.h dist/$(DISTNAME)/include/
	cp -p tests/Makefile tests/*.out.ok tests/*.apl tests/*.txt dist/$(DISTNAME)/tests/
	cp -p MIT_LICENSE.md dist/$(DISTNAME)/doc/MIT_LICENSE
	cp -p doc/README_BIN dist/$(DISTNAME)/doc/README
	(cd dist; tar -czf $(DISTNAME).tgz $(DISTNAME))

.PHONY: test
test: aplt Makefile
	$(MAKE) -C tests test

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
