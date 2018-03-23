BINFILES=TestReg

test: TestReg
	./TestReg

TestReg: TestReg.hs Reg.hs RegExtra.hs Mon.hs
	ghc -O2 --make $@

clean:: hsclean
	-rm -f *~

hsclean::
	-rm -f *.hi *.o $(BINFILES)

