pico: pico.ml
	ocamlc -o pico pico.ml

test.out: pico.ml test.ml
	ocamlc -o test.out pico.ml test.ml

.PHONY: test
test: test.out
	./test.out

.PHONY: clean
clean:
	rm -f *.cmx *.cmi *.o *.cmo pico test.out
