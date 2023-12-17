all: clean test-advanced test-simple

compile-advanced:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 ghc -main-is Day17.Advanced.main -o Day17/advanced Day17/Advanced.hs

compile-simple:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day17.Simple.main -o Day17/simple Day17/Simple.hs

dev-advanced:
	cd ../; ghcid Day17/Advanced.hs --setup ":set args Day17/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid Day17/Simple.hs --setup ":set args Day17/input/simple-example.txt" --test main

run-advanced: compile-advanced
	cd ../; ./advanced Day17/input/advanced.txt

run-simple: compile-simple
	cd ../; ./simple Day17/input/simple.txt

pretest:
	mkdir -p test/

test: test-advanced test-simple

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
