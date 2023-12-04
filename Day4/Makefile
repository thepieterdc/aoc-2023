all: clean test-advanced test-simple

dev-advanced:
	cd ../; ghcid Day4/Advanced.hs --setup ":set args Day4/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid Day4/Simple.hs --setup ":set args Day4/input/simple-example.txt" --test main

run-advanced:
	cd ../; runhaskell Day4/Advanced.hs Day4/input/advanced.txt

run-simple:
	cd ../; runhaskell Day4/Simple.hs Day4/input/simple.txt

pretest:
	mkdir -p test/

test: test-advanced test-simple

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
