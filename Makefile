

all: ebin/ebok.beam ebin/backend.beam

ebin/ebok.beam: src/ebok.erl
	mkdir -p ebin
	erlc -o ebin $<

ebin/backend.beam: src/backend.erl
	mkdir -p ebin
	erlc -o ebin $<
