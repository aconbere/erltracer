ERLC ?= $(shell which erlc)

all:
	$(ERLC) -o ebin/ src/*.erl

clean:
	rm -f ebin/*.beam

dist-clean: clean
	find . \( -name \*~ -o -name *.swp \) -exec rm -f {} \;
