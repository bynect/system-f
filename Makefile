GHC=ghc
CPP=-DPACK_TLAM -DPACK_FORALL -DSUGAR_LET -DSHOW_UNICODE

all: Main

Main: $(wildcard *.hs)
	$(GHC) $(CPP) $^

clean:
	rm -f *.hi *.o Main
