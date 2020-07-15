VERSION := $(shell cat VERSION)

MINIFY?=true

all:	proactive-${VERSION}/lib/proactive.js proactive-${VERSION}/lib/proscript.wasm

node_modules/uglify-js:
	npm install uglify-es

node_modules/browserify:
	npm install browserify

proscript2/proscript.js:
	make -C proscript2

proactive-${VERSION}/lib/proactive.js:	src/proactive.js src/on_server.js src/constants.js src/dot.js src/get_this.js src/prolog_utilities.js node_modules/uglify-js node_modules/browserify proscript2/proscript.js
# We have to disable warnings here because emscripten generates output containing a HUGE amount of unused vars and functions
# and uglify produces pages and pages of warnings about them if we dont stop it
	mkdir -p proactive-${VERSION}/lib
ifeq ($(MINIFY),true)
	NODE_PATH=. node_modules/browserify/bin/cmd.js src/proactive.js | node_modules/uglify-es/bin/uglifyjs  -m  > $@
else
	NODE_PATH=. node_modules/browserify/bin/cmd.js src/proactive.js > $@
endif

proactive-${VERSION}/lib/proscript.wasm: proscript2/proscript.wasm
	cp proscript2/proscript.wasm $@
