all: clean test-advanced test-simple

compile-advanced:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day16.Advanced.main -o Day16/advanced Day16/Advanced.hs 1>&2

compile-simple:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day16.Simple.main -o Day16/simple Day16/Simple.hs 1>&2

dev-advanced:
	cd ../; ghcid Day16/Advanced.hs --setup ":set args Day16/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid Day16/Simple.hs --setup ":set args Day16/input/simple-example.txt" --test main

run-advanced: compile-advanced
	./advanced input/advanced.txt

run-simple: compile-simple
	./simple input/simple.txt

pretest:
	mkdir -p test/

test: test-advanced test-simple

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
