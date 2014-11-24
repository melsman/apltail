TESTS=test test1 test2 test3 test4 test5 test6 test7 test9 test10 \
      test11 test12 test13 test14 test15 test16 test19 signal \
      test21 test28 residue mult0 mult sierpinski0 slashbar primes0 \
      compress primes replicate cons boolean innerLegrand5.3 quadassign \
      opr1 opr2 opr3 fun2 take drop vrev vrot rot rotfirst first life pow \
      vec zfunc repl ceilfloormaxmin easter eacheaster else chars shape \
      innerproduct

APLFILES=$(TESTS:%=%.apl)
TLFILES=$(APLFILES:%.apl=%.tl)
RESFILES=$(APLFILES:%.apl=%.res) $(APLFILES:%.apl=%.resn)

.PHONY: all
all: $(TLFILES) test

%.tl: %.apl Makefile ../aplt
	../aplt -c -o $@ ../prelude.apl $<

%.out: %.apl Makefile ../aplt
	../aplt ../prelude.apl $< > $@

%.outn: %.apl Makefile ../aplt
	../aplt -noopt ../prelude.apl $< > $@

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

%.resn: %.outn
	@(diff -aq $< $*.out.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*.apl (no optimization): OK" > $@ \
         ; else \
             if [ -e $*.out.ok ]; then \
                echo "Test $*.apl: *** ERR: file $< differs from $*.out.ok ***" > $@ \
             ; else \
                echo "Test $*.apl: *** ERR: file $*.out.ok does not exist ***" > $@ \
             ; fi \
         ; fi)

.PHONY: test
test: $(RESFILES)
	@cat $(RESFILES)
	@echo "-------T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(RESFILES) | wc -l` /`grep "Test" $(RESFILES) | wc -l`"
	@echo "-------------------------------------"


clean:
	rm -f *.tl *.out *.res *.resn *.outn *~
