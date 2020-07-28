VERSION := $(shell cat VERSION)

MINIFY?=true

all:	package
.PHONY: package

package: proactive-${VERSION}/lib/proactive.js proactive-${VERSION}/lib/proscript.wasm proactive-${VERSION}/lib/boilerplate.pl proactive-${VERSION}/src/jsx.pl proactive-${VERSION}/src/proactive.pl
	zip -r proactive-${VERSION}.zip proactive-${VERSION}

node_modules/uglify-js:
	npm install uglify-es

node_modules/browserify:
	npm install browserify

proscript2/proscript.js:
	make -C proscript2

proactive-${VERSION}/lib/proactive.js:	src/proactive.js src/on_server.js src/t7_now.js src/constants.js src/dot.js src/get_this.js src/bubble_event.js src/prolog_utilities.js src/message_service.js src/message_listener.js node_modules/uglify-js node_modules/browserify proscript2/proscript.js proactive-${VERSION}/lib
# We have to disable warnings here because emscripten generates output containing a HUGE amount of unused vars and functions
# and uglify produces pages and pages of warnings about them if we dont stop it
ifeq ($(MINIFY),true)
	NODE_PATH=. node_modules/browserify/bin/cmd.js src/proactive.js | node_modules/uglify-es/bin/uglifyjs  -m  > $@
else
	NODE_PATH=. node_modules/browserify/bin/cmd.js src/proactive.js > $@
endif

SRC_FILES=proactive-${VERSION}/src/proactive.pl proactive-${VERSION}/src/jsx.pl


proactive-${VERSION}/lib/boilerplate.pl: src/boilerplate.pl proactive-${VERSION}/lib
	cp $< $@

$(SRC_FILES): proactive-${VERSION}/src/%: src/% proactive-${VERSION}/src
	cp $< $@

proactive-${VERSION}/src:
	mkdir -p $@

proactive-${VERSION}/lib:
	mkdir -p $@




proactive-${VERSION}/lib/proscript.wasm: proscript2/proscript.wasm
	cp proscript2/proscript.wasm $@

clean:
	rm -rf proactive-*
