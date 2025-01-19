koka:
	dune build

test1: koka
	./test.sh -1 ./kokac.exe

test2: koka
	./test.sh -2 ./kokac.exe

test3: koka
	./test.sh -3 ./kokac.exe

clean:
	rm -rf ./_build
	rm -rf kokak.exe

temp_test: kokac.exe
	dune exec ./kokac.exe temp_test.koka
	gcc -g -no-pie temp_test.s
	./a.out
	

.PHONY: koka test1 test2