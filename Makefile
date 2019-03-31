all: build

build: .stack-work
	stack build

run: build
	stack exec vty

test: build
	stack test

clean:
	stack clean

.PHONY: all clean
