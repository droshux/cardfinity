.PHONY= update build optim

all: update build optim

# js: update-js build-js

update:
	wasm32-wasi-cabal update

repl: update
	wasm32-wasi-cabal repl cardfinity-designer -finteractive --repl-options='-fghci-browser -fghci-browser-port=8080'

watch:
	ghciwatch --after-startup-ghci :main --after-reload-ghci :main --watch . --debounce 50ms --command 'wasm32-wasi-cabal repl cardfinity-designer -finteractive --repl-options="-fghci-browser -fghci-browser-port=8080"'

build:
	wasm32-wasi-cabal build cardfinity-designer
	rm -rf cardfinity-designer/public
	cp -r cardfinity-designer/static cardfinity-designer/public
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin cardfinity-designer | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output cardfinity-designer/public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) cardfinity-designer/public/

optim:
	wasm-opt -all -O2 cardfinity-designer/public/cardfinity-designer.wasm -o cardfinity-designer/public/cardfinity-designer.wasm
	wasm-tools strip -o cardfinity-designer/public/cardfinity-designer.wasm cardfinity-designer/public/cardfinity-designer.wasm

serve:
	http-server public

clean:
	rm -rf dist-newstyle cardfinity-designer/public

update-js:
	# cabal update --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

build-js:
	# cabal build --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg
	# cp -v ./dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/*/x/app/build/app/app.jsexe/all.js .
	# rm -rf public
	# cp -rv static public
	# bunx swc ./all.js -o public/index.js
