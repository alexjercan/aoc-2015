all:
	cabal build
	cabal test

Day%.hs:
	cabal run Aoc2015 $*

%.hs:
	cabal test

