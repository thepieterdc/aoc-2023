all: clean test-advanced test-simple

compile-advanced:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day__day__.Advanced.main -o Day__day__/advanced Day__day__/Advanced.hs

compile-simple:
	cd ../; ghc -funfolding-use-threshold=16 -O2 -optc-O3 -main-is Day__day__.Simple.main -o Day__day__/simple Day__day__/Simple.hs

dev-advanced:
	cd ../; ghcid Day__day__/Advanced.hs --setup ":set args Day__day__/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid Day__day__/Simple.hs --setup ":set args Day__day__/input/simple-example.txt" --test main

run-advanced: compile-advanced
	./advanced input/advanced.txt

run-simple: compile-simple
	./simple input/simple.txt

pretest:
	mkdir -p test/

profile-simple:

test: test-advanced test-simple

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
