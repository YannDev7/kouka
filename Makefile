koka:
	dune build

test1: koka
	./test.sh -1 ./koka.exe

test2: koka
	./test.sh -2 ./koka.exe

clean:
	rm -rf ./_build

.PHONY: koka test