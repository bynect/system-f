GHC=ghc
CPP=-DPACK_TLAM -DPACK_FORALL -DSUGAR_LET

all: Main

Main: $(wildcard *.hs)
	$(GHC) $(CPP) $^

clean:
	rm -f *.hi *.o Main
