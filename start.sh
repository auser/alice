#!/bin/sh
cd `dirname $0`
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -sname alice -s reloader -boot rest_app