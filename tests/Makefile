TESTS=test test1 test2 test3 test4 test5 test6 test7 test9 test10 \
      test11 test12 test13 test14 test15 test16 test19 signal \
      test21 test28 residue mult0 mult sierpinski0 slashbar primes0 \
      compress primes replicate cons boolean innerLegrand5.3 quadassign \
      opr1 opr2 opr3

APLFILES=$(TESTS:%=%.apl)
TLFILES=$(APLFILES:%.apl=%.tl)
RESFILES=$(APLFILES:%.apl=%.res)

.PHONY: all
all: $(TLFILES) test

%.tl: %.apl Makefile ../aplt
	../aplt -c -o $@ ../prelude.apl $<

%.out: %.apl Makefile ../aplt
	../aplt ../prelude.apl $< > $@

%.res: %.out
	@(diff -aq $< $<.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*.apl: OK" > $@ \
         ; else \
             if [ -e $<.ok ]; then \
                echo "Test $*.apl: *** ERR: file $< differs from $<.ok ***" > $@ \
             ; else \
                echo "Test $*.apl: *** ERR: file $<.ok does not exist ***" > $@ \
             ; fi \
         ; fi)

.PHONY: test
test: $(RESFILES)
	@cat $(RESFILES)

clean:
	rm -f *.tl *.out *.res
