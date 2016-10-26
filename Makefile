#MLKIT=SML_LIB=/Users/mael/gits/mlkit /Users/mael/gits/mlkit/bin/mlkit
#MLCOMP ?= $(MLKIT) -mlb-path-map $(HOME)/.mlkit/mlb-path-map

MLCOMP ?= mlkit

#MLCOMP ?= mlton -mlb-path-map $(HOME)/.mlton/mlb-path-map

FILES=src/flags.sml src/flags.mlb src/aplt.sml src/aplt.mlb \
  src/apl2tail.mlb src/Apl2Tail.sml src/Tail2Laila.sml \
  src/util.sig src/util.sml src/util.mlb \
  $(shell ls -1 src/tail/*.sig src/tail/*.sml src/tail/*.mlb) \
  $(shell ls -1 src/il/*.sig src/il/*.sml src/il/*.mlb) \
  $(shell ls -1 src/laila/*.sig src/laila/*.sml src/laila/*.mlb)

SMACKAGE ?= $(HOME)/.smackage/lib
APLPARSE_LIB ?= $(SMACKAGE)/aplparse/v2.8
PREFIX ?= .
DESTDIR ?= $(PREFIX)/dist

.PHONY: all
all: aplt

aplt: src/aplt.mlb $(FILES) src/aplt.sml
	APLPARSE_LIB=$(APLPARSE_LIB) $(MLCOMP) -output $@ $<
#	$(MLCOMP) -mlb-path-var 'APLPARSE_LIB $(APLPARSE_LIB)' -output $@ $<

.PHONY: aplt-mlton
aplt-mlton: src/aplt.mlb
	$(MLCOMP) -mlb-path-var 'APLPARSE_LIB $(APLPARSE_LIB)' -output aplt $<

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

.PHONY: clean
clean: Makefile
	find . -name '*~' | xargs rm -f
	find . -name 'MLB' | xargs rm -rf
	find . -name 'run' | xargs rm -f
	rm -f aplt
	$(MAKE) -C tests clean
