# Makefile k projektu do předmětu FLP
# Daniel Pátek (xpatek08)
# 2022-03-21

.PHONY: flp21-fun

all: flp21-fun

flp21-fun:
	ghc -o flp21-fun main.hs
	rm -rf *.o *.hi tests/*.temp

clean:
	rm -rf flp21-fun *.o *.hi xpatek08.zip

zip:
	zip xpatek08.zip makefile Parser.hs Simplify.hs Types.hs README main.hs