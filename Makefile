SHELL := /bin/bash

all: test configure
	cabal build
	cp dist/build/LorenzAttractor/LorenzAttractor .
	cp dist/build/LorenzAttractor/LorenzAttractor LorenzAttractor-HW2-AdamC
	./LorenzAttractor
test:
	if [[ "$$(which cabal)" == "" ]]; then echo cabal does not exist. Install it with \'sudo apt-get install cabal-install\'; exit 1; fi
configure:
	cabal update
	cabal install cabal
	cabal install --only-dependencies
	cabal configure
clean:
	cabal clean
	- rm -fr LorenzAttractor dist/build/LorenzAttractor
	- rm -fr LorenzAttractor dist
	