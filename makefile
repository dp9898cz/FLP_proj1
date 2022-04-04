# Makefile k projektu do předmětu FLP
# Daniel Pátek (xpatek08)
# 2022-03-21

.PHONY: simplify-bkg

all: simplify-bkg

simplify-bkg:
	ghc -o simplify-bkg main.hs
	rm -rf *.o *.hi tests/*.temp

clean:
	rm -rf simplify-bkg *.o *.hi xpatek08.zip

zip:
	zip xpatek08.zip makefile Parser.hs Simplify.hs Types.hs README main.hs