#!/bin/sh
cd `dirname $0`
make
make make_boot
make wonderland

echo '
Commands:
sudo rabbitmq-server
/bin/sh start.sh
rstakeout "make compile" "*/**/*"
'