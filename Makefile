MLCOMP ?= mlton -mlb-path-map $(HOME)/.mlton/mlb-path-map
FILES=apl2tail.mlb Apl2Tail.sml $(shell ls -1 tail/*.sig tail/*.sml tail/*.mlb)
SMACKAGE ?= $(HOME)/.smackage/lib
APLPARSE_LIB ?= $(SMACKAGE)/aplparse/v2.2

.PHONY: all
all: aplt

aplt: aplt.mlb $(FILES) aplt.sml Makefile
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
	rm -rf aplt
	$(MAKE) -C tests clean 
