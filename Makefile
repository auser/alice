LIBDIR		= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION		= 0.0.1
CC  			= erlc
ERL     	= erl
EBIN			= ebin
CFLAGS  	= -I include -pa $(EBIN)
COMPILE		= $(CC) $(CFLAGS) -o $(EBIN)
EBIN_DIRS = $(wildcard deps/*/ebin)

all: mochi ebin compile
all_boot: all make_boot
start: all start_all

mochi:
	@(cd deps/mochiweb;$(MAKE))

compile:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

edoc:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'
	
make_boot:
	(cd ebin; erl -pa ebin -noshell -run make_boot write_scripts rest_app)

start_all:
	(cd ebin; erl -pa ebin -noshell -sname alice -boot alice)

ebin:
	@mkdir ebin

clean:
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump ebin/*.boot ebin/*.rel ebin/*.script