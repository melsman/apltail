TESTS=test test1 test2 test3 test4 test5 test6 test7 test9 test10 \
      test11 test12 test13 test14 test15 test16 test19 signal \
      test21 test28 residue mult0 mult sierpinski0 slashbar primes0 \
      compress primes replicate cons boolean innerLegrand5.3 quadassign \
      opr1 opr2 opr3 fun2 take drop vrev vrot rot rotfirst first life powscl pow powmat \
      vec zfunc repl ceilfloormaxmin easter eacheaster else chars shape \
      innerproduct sign ln circ timespi blackscholes pi readfile \
      readintvecfile tax dtransp abcd mean idx tup binaryops fib10 sierpinski \
      idxassign

APLFILES=$(TESTS:%=%.apl)
TLFILES=$(APLFILES:%.apl=%.tl)
RESFILES=$(APLFILES:%.apl=%.res) $(APLFILES:%.apl=%.resn)
OUTVFILES=$(APLFILES:%.apl=%.outv)

PRELUDE=../lib/prelude.apl
APLT=../aplt

.PHONY: all
all: $(TLFILES) test

%.tl: %.apl Makefile ../aplt
	$(APLT) -s_tail -c -o $@ $(PRELUDE) $<

%.out: %.apl Makefile ../aplt
	$(APLT) -s_tail -silent $(PRELUDE) $< > $@

%.outn: %.apl Makefile ../aplt
	$(APLT) -s_tail -noopt -silent $(PRELUDE) $< > $@

%.outv: %.apl Makefile ../aplt
	$(APLT) -s_tail -p_tail -p_types -noopt $(PRELUDE) $< > $@

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
	rm -f c/*.out c/*.c c/*~ *~ c/*.exe *.time *.dyatime *.comptime *.outd *.outb


################### Testing Laila (C) Backend ###################

CTESTS=test test1 test2 test3 test4 test5 test6 test7 test9 test10 \
       test11 test12 test13 test14 test15 test16 test19 signal \
       test21 test28 residue mult0 mult slashbar primes0 \
       compress primes cons boolean innerLegrand5.3 quadassign \
       opr1 opr2 opr3 fun2 take drop vrev vrot rot rotfirst first powscl \
       vec zfunc ceilfloormaxmin easter eacheaster else \
       innerproduct sign mean powmat life pow fib10 timespi ln \
       chars repl replicate shape tup circ abcd dtransp tax binaryops \
       pi blackscholes 

#      sierpinski0 sierpinski readfile \
#      readintvecfile idx

CRESFILES=$(CTESTS:%=%.resc)

.PHONY: testc
testc: $(CRESFILES)
	@cat $(CRESFILES)
	@echo "------- C Backend --- T E S T --- R E P O R T-------"
	@echo "Tests succeeded:   `grep "OK" $(CRESFILES) | wc -l` /`grep "Test" $(CRESFILES) | wc -l`"
	@echo "Test errors:       `grep "ERR" $(CRESFILES) | wc -l` /`grep "Test" $(CRESFILES) | wc -l`"
	@echo "----------------------------------------------------"

c/%.c: %.apl Makefile
	$(APLT) -c -oc $@ $(PRELUDE) $<

c/%.exe: c/%.c
	gcc --std=c99 -O3 -o $@ -lm $<

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

###### Benchmarking the Laila (C) Backend against Dyalog interpreter ######
#
# This benchmarking assumes that dyalog is installed together with the 
# rundyalog script (with IO<-1) being available in the user's PATH. 
#

TIME=/usr/bin/time
#TIME=gtime

BENCH=pi2 life2 primes2 easter3000 blacksch

TIMEFILES=$(BENCH:%=%.time)
DYATIMEFILES=$(BENCH:%=%.dyatime)
COMPTIMEFILES=$(BENCH:%=%.comptime)

.PHONY: bench
bench: $(COMPTIMEFILES)
	@echo
	@echo "----- B E N C H M A R K --- R E P O R T-----"
	@echo 'Benchmark        Dyalog        Laila'
	@cat $(COMPTIMEFILES)
	@echo "--------------------------------------------"

c/%.c: %.apl Makefile
	$(APLT) -c -oc $@ $(PRELUDE) $<

c/%.exe: c/%.c
	gcc --std=c99 -O3 -o $@ -lm $<

%.time: c/%.exe
	$(TIME) -o $@ -f "%Us" $< > $*.outb

%.dyatime: %.apl
	$(TIME) -o $@ -f "%Us" rundyalog $< > $*.outd

%.comptime: %.dyatime %.time
	printf '%-15s %7s      %7s\n' "$*.apl" `cat $*.dyatime | tr -d '\n'` `cat $*.time | tr -d '\n'` > $@
