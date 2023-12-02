all: clean test-advanced test-simple

dev-advanced:
	cd ../; ghcid Day2/Advanced.hs --setup ":set args Day2/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid Day2/Simple.hs --setup ":set args Day2/input/simple-example.txt" --test main

run-advanced:
	cd ../; runhaskell Day2/Advanced.hs Day2/input/advanced.txt

run-simple:
	cd ../; runhaskell Day2/Simple.hs Day2/input/simple.txt

pretest:
	mkdir -p test/

test: test-advanced test-simple

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
