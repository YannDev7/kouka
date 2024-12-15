koka:
	dune build

test1: koka
	./test.sh -1 ./kokac.exe

test2: koka
	./test.sh -2 ./kokac.exe

clean:
	rm -rf ./_build
	rm -rf kokak.exe

.PHONY: koka test1 test2