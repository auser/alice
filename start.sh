#!/bin/sh
cd `dirname $0`
if [[ ! -f ebin/rest_app.boot ]]; then
	make all_boot
fi
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -sname alice -s reloader -boot rest_app $1