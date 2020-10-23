# it's half done, I couldn't make it work on linux

.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $< 

MODS = port_example

all: ${MODS:%=%.beam} port_example port_example_drv.so
	@erl -noshell -s unit_test start

port_example: port_example.c erl_comm.c port_example_driver.c
	gcc -o port_example port_example.c erl_comm.c port_example_driver.c

port_example_drv.so: port_example.c
	gcc -I /usr/lib/erlang/usr/include\
		-o port_example_drv.so -fPIC\
		port_example.c

clean:
	rm port_example port_example_drv.so *.beam
