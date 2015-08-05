MLCOMP ?= mlton -mlb-path-map $(HOME)/.mlton/mlb-path-map
FILES=src/flags.sml src/flags.mlb src/aplt.sml src/aplt.mlb \
  src/commentparse.mlb src/CommentParse.sml \
  src/apl2tail.mlb src/Apl2Tail.sml src/Tail2Laila.sml \
  src/util.sig src/util.sml src/util.mlb \
  $(shell ls -1 src/tail/*.sig src/tail/*.sml src/tail/*.mlb) \
  $(shell ls -1 src/il/*.sig src/il/*.sml src/il/*.mlb) \
  $(shell ls -1 src/laila/*.sig src/laila/*.sml src/laila/*.mlb)

SMACKAGE ?= $(HOME)/.smackage/lib
#APLPARSE_LIB ?= $(SMACKAGE)/aplparse/v2.5
APLPARSE_LIB ?= $(HOME)/Documents/research/tail/aplparse

.PHONY: all
all: aplt

aplt: src/aplt.mlb $(FILES) src/aplt.sml Makefile
	$(MLCOMP) -mlb-path-var 'APLPARSE_LIB $(APLPARSE_LIB)' -output $@ $<

.PHONY: install
install:
	cp -p aplt $(DESTDIR)/bin/

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
