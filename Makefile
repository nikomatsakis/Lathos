BIN=ebin

.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -o ${BIN} -W $<
	
.yrl.erl:
	erlc -o $@ -W $<
	chmod u-w $@
	
ERL = erl -boot start_clean

MODS =	src/lathos src/lathos_tests src/lathos_serve \
		src/lathos_parse src/lathos_parse_tests \
		src/pico_http_server src/pico_socket_server \
		src/pico_utils 

all: test

clean:
	rm -rf bin/*

compile: ${MODS:%=%.beam} 

test: compile
	erl -noshell -pa ${BIN} -s lathos_tests test -s init stop
	erl -noshell -pa ${BIN} -s lathos_parse_tests test -s init stop

run_server: compile
	erl -pa ebin -s lathos_serve start
	
post_test_tuples:
	curl --data-binary '@test_tuple1.post' http://localhost:4999/create_node
	@echo
	curl --data-binary '@test_tuple2.post' http://localhost:4999/create_node
	@echo