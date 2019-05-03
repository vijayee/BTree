build:
	mkdir -p build
test: build
	mkdir -p build/test
test/BTree: test BTree/*.pony BTree/test/*.pony
	stable fetch
	stable env ponyc BTree/test -o build/test --debug
test/execute: test/BTree
	./build/test/test
clean:
	rm -rf build

.PHONY: clean test
