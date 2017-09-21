all:
	-hlint lint -j4 --report library executable test-suite
	stack -j4 build
	stack test
	stack bench
	stack -j4 haddock
	stack exec wavetoy2

clean:
	rm -f report.html
	stack clean
