all: clean test-advanced test-simple

compile-advanced:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day15.Advanced.main -o Day15/advanced Day15/Advanced.hs

compile-simple:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day15.Simple.main -o Day15/simple Day15/Simple.hs


dev-advanced:
	cd ../; ghcid Day15/Advanced.hs --setup ":set args Day15/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid Day15/Simple.hs --setup ":set args Day15/input/simple-example.txt" --test main

run-advanced: compile-advanced
	./advanced input/advanced.txt

run-simple: compile-simple
	./simple input/simple.txt

pretest:
	mkdir -p test/
	cabal update
	cabal install --lib split

test: test-advanced test-simple

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
