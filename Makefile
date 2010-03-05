.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -o bin -W $<
	
.yrl.erl:
	erlc -o bin -W $<
	
ERL = erl -boot start_clean

MODS = src/lathos

all: compile

clean:
	rm -rf bin/*

compile: ${MODS:%=%.beam} 

test: compile
	erl -noshell -pa bin -pa test/bin -s test_suite test