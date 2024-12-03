koka:
	dune build

test: koka
	./test.sh -1 ./koka.exe

clean:
	rm -rf ./_build

.PHONY: koka test