TESTS=test test1 test2 test3 test4 test5 test6 test7 test9 test10 \
      test11 test12 test13 test14 test15 test16 test19 signal \
      test21 test28 residue mult0 mult sierpinski0 slashbar primes0 \
      compress primes replicate cons boolean innerLegrand5.3 quadassign \
      opr1 opr2 opr3 fun2 take drop vrev vrot rot rotfirst first life powscl pow \
      vec zfunc repl ceilfloormaxmin easter eacheaster else chars shape \
      innerproduct sign ln circ timespi blackscholes pi readfile \
      readintvecfile tax dtransp abcd mean idx tup binaryops

APLFILES=$(TESTS:%=%.apl)
TLFILES=$(APLFILES:%.apl=%.tl)
RESFILES=$(APLFILES:%.apl=%.res) $(APLFILES:%.apl=%.resn)
OUTVFILES=$(APLFILES:%.apl=%.outv)

.PHONY: all
all: $(TLFILES) test

%.tl: %.apl Makefile ../aplt
	../aplt -s_tail -c -o $@ ../prelude.apl $<

%.out: %.apl Makefile ../aplt
	../aplt -s_tail -silent ../prelude.apl $< > $@

%.outn: %.apl Makefile ../aplt
	../aplt -s_tail -noopt -silent ../prelude.apl $< > $@

%.outv: %.apl Makefile ../aplt
	../aplt -s_tail -p_tail -p_types -noopt ../prelude.apl $< > $@

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

outv: $(OUTVFILES)


clean:
	rm -f *.tl *.out *.res *.resn *.outn *~ *.outv *.outc *.resc
	rm -f c/*.out c/*.c c/*~ *~ c/*.exe


################### Testing Laila (C) Backend ###################

CTESTS=test test1 test2 test3 test4 test5 test6 test7 test9 test10 \
       test11 test12 test13 test14 test15 test16 test19 signal \
       test21 test28 residue mult0 mult slashbar primes0 \
       compress primes cons boolean innerLegrand5.3 quadassign \
       opr1 opr2 opr3 fun2 take drop vrev vrot rot rotfirst first powscl \
       vec zfunc ceilfloormaxmin easter eacheaster else \
       innerproduct sign mean
#      life replicate pow repl chars sierpinski0 shape ln circ timespi blackscholes pi readfile \
#      readintvecfile tax dtransp abcd idx tup binaryops

CRESFILES=$(CTESTS:%=%.resc)

.PHONY: testc
testc: $(CRESFILES)
	@cat $(CRESFILES)
	@echo "------- C Backend --- T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(CRESFILES) | wc -l` /`grep "Test" $(CRESFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(CRESFILES) | wc -l` /`grep "Test" $(CRESFILES) | wc -l`"
	@echo "----------------------------------------------------"

c/%.c: %.apl Makefile
	../aplt -c -oc $@ ../prelude.apl $<

c/%.exe: c/%.c
	gcc --std=c99 -o $@ $<

%.outc: c/%.exe
	$< > $@

%.resc: %.outc
	@(diff -aq $< $*.out.ok > /dev/null 2>&1; \
         if [ $$? -eq 0 ]; then \
             echo "Test $*.apl (c backend): OK" > $@ \
         ; else \
             if [ -e $*.out.ok ]; then \
                echo "Test $*.apl: *** ERR: file $< differs from $*.out.ok ***" > $@ \
             ; else \
                echo "Test $*.apl: *** ERR: file $*.out.ok does not exist ***" > $@ \
             ; fi \
         ; fi)
