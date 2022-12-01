main.js: main.ml
	dune build --profile=prod
	cp _build/default/main.bc.js main.js
	chmod +rw main.js

clean:
	rm -f main.js
	dune clean