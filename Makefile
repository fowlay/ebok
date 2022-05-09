

all: ebin/ebok.beam ebin/backend.beam ebin/xml_io.beam

ebin/ebok.beam: src/ebok.erl
	mkdir -p ebin
	erlc -o ebin $<

ebin/backend.beam: src/backend.erl
	mkdir -p ebin
	erlc -o ebin $<

ebin/xml_io.beam: src/xml_io.erl
	mkdir -p ebin
	erlc -o ebin $<
