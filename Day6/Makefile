all: clean test-advanced test-simple

dev-advanced:
	cd ../; ghcid Day6/Advanced.hs --setup ":set args Day6/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid Day6/Simple.hs --setup ":set args Day6/input/simple-example.txt" --test main

run-advanced:
	cd ../; runhaskell Day6/Advanced.hs Day6/input/advanced.txt

run-simple:
	cd ../; runhaskell Day6/Simple.hs Day6/input/simple.txt

pretest:
	mkdir -p test/

test: test-advanced test-simple

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
