BIN=ebin

.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -o ${BIN} -W $<
	
.yrl.erl:
	erlc -o ${BIN} -W $<
	
ERL = erl -boot start_clean

MODS = 	src/lathos src/lathos_tests src/lathos_serve \
		src/pico_http_server src/pico_socket_server \
		src/pico_utils

all: test

clean:
	rm -rf bin/*

compile: ${MODS:%=%.beam} 

test: compile
	erl -noshell -pa ${BIN} -s lathos_tests test -s init stop
