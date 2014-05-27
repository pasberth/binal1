BINAL=./dist/build/binal/binal
ESCODEGEN=./node_modules/escodegen/bin/esgenerate.js
CABAL=cabal
NPM=npm

.PHONY: compile
compile:
	$(CABAL) sandbox init
	$(CABAL) install --only-dependencies --enable-tests
	$(CABAL) build
	$(NPM) install escodegen
	$(NPM) install ./binal-runtime

%.js: %.binal
	$(BINAL) $^
	$(ESCODEGEN) $(patsubst %.binal, %.json, $^) > $@
