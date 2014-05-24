BINAL=./dist/build/binal/binal
ESCODEGEN=./node_modules/escodegen/bin/esgenerate.js

%.js: %.binal
	$(BINAL) $^
	$(ESCODEGEN) $(patsubst %.binal, %.json, $^) > $@
