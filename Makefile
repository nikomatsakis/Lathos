EUNIT=eunit/Makefile
EUNITBIN=eunit/ebin
BIN=ebin

.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -pa ${EUNITBIN} -o ${BIN} -W $<
	
.yrl.erl:
	erlc -o ${BIN} -W $<
	
ERL = erl -boot start_clean

MODS = src/lathos src/lathos_tests

all: test

clean:
	rm -rf bin/*

${EUNIT}:
	svn co http://svn.process-one.net/contribs/trunk/eunit eunit
	cd eunit; make

compile: ${EUNIT} ${MODS:%=%.beam} 

test: compile
	erl -noshell -pa ${BIN} -pa ${EUNITBIN} -s lathos_tests test -s init stop
