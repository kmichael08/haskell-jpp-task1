BINFILES=TestReg myTest

test: TestReg
	./TestReg

mytest: myTest
	./myTest

TestReg: TestReg.hs Reg.hs RegExtra.hs Mon.hs
	ghc -O2 --make $@

myTest: myTest.hs Reg.hs RegExtra.hs Mon.hs
	ghc -O2 --make $@

clean:: hsclean
	-rm -f *~

hsclean::
	-rm -f *.hi *.o $(BINFILES)

