.PHONY: run test setup pkg

build:
	stack build --bench --no-run-benchmarks

test: build
	stack test

singletest: build
	stack test --ta '-p "Lib"'

test-trace: build
	stack test --trace

bench:
	stack bench

run: build
	stack exec pie-haskell-exe

fmt:
	brittany --write-mode=inplace *.hs
	brittany --write-mode=inplace **/*.hs

setup:
	stack setup
	stack install brittany

lint:
	hlint .

