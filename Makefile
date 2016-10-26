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

.PHONY: dist
dist:
	rm -rf dist
	mkdir dist
	mkdir dist/apltail
	mkdir dist/apltail/bin
	mkdir dist/apltail/lib
	mkdir dist/apltail/include
	mkdir dist/apltail/tests
	mkdir dist/apltail/doc
	cp -p aplt dist/apltail/bin/
	cp -p lib/prelude.apl dist/apltail/lib/
	cp -p include/apl.h dist/apltail/include/
	cp -p tests/Makefile tests/*.out.ok tests/*.apl tests/*.txt dist/apltail/tests/
	cp -p MIT_LICENSE.md dist/apltail/doc/MIT_LICENSE
	cp -p doc/README_BIN dist/apltail/doc/README
	(cd dist; tar -czf apltail.tgz apltail)

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
