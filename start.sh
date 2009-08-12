#!/bin/sh
cd `dirname $0`
if [[ ! -f ebin/rest_app.boot ]]; then
	make all_boot
fi
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -name alice -s reloader -boot alice $*