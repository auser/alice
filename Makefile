LIBDIR	= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION	= 0.0.1
CC  		= erlc
EBIN		= ebin
CFLAGS  = -I include -pa $(EBIN)

all: ebin utils rest make_boot

start: all start_all

utils:
	$(CC) $(CFLAGS) -o $(EBIN) src/utils/*.erl
	
rest:
	$(CC) $(CFLAGS) -o $(EBIN) src/REST/*.erl

make_boot:
	(cd ebin; erl -pa ebin -noshell -run make_boot write_scripts rest_app)

start_all:
	(cd ebin; erl -pa ebin -noshell -boot alice)

ebin:
	@mkdir ebin

clean:
	(rm ebin/*.beam;rm ebin/erl_crash.dump;rm erl_crash.dump; rm ebin/*.boot; rm ebin/*.rel; rm ebin/*.script)