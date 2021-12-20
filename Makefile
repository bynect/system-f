GHC=ghc
CPP=-DPACK_FORALL

all: Main

Main: $(wildcard *.hs)
	$(GHC) $(CPP) $^

clean:
	rm -f *.hi *.o Main
