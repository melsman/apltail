TESTS=test test2 test3 test4 test5 test6 test7 test9 test10 test11 test12 test13 test14 test15 test16 test19 signal test21

#TESTS=test test2 test3 test4 test5 test6 test7 test9 test10 test11 test12 test13 test14 test16 signal

APLFILES=$(TESTS:%=%.apl)
TLFILES=$(APLFILES:%.apl=%.tl)

.PHONY: all
all: $(TLFILES)

%.tl: %.apl Makefile ../aplt
	../aplt -c -o $@ ../prelude.apl $<

clean:
	rm -f *.tl
