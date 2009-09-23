#!/bin/sh +x

alice_dir=`dirname $0`

while getopts "n:p:l:s:r:a:hd" o ; do
  case $o in
    n) name=$OPTARG;;
    s) sname=$OPTARG;;
    p) port=$OPTARG;;
    l) log=$OPTARG;;
    r) rabbithost=$OPTARG;;
    d) detached="true";;
    a) alice_directory=$OPTARG;;
    h) echo "Usage:  start [options]
Options:
-n          name
-s          sname
-a          Alice directory
-p          Port
-d          Run in Daemon mode
-r          Rabbit host
-l          Log directory to log
-h          Show this help screen"
      exit 0
      ;;
  esac
done

if [ -z ${name} ]; then
  if [ -z ${sname} ]; then
    name_directive="-sname alice";
  else
    name_directive="-sname ${sname}";
  fi
else
  name_directive="-name ${name}";
fi
if [ ! -z ${port} ]; then
  port_directive="port ${port}"
fi
if [ ! -z ${log} ]; then
  log_directive="log_path '${log}/alice.${port}.log'";
fi
if [ ! -z ${detached} ]; then
  daemon_directive="-detached"
fi
if [ ! -z ${rabbithost} ]; then
  rabbithost_directive="rabbithost ${rabbithost}"
fi

echo "Starting alice"
echo ""
echo " node named: ${name_directive}"
echo " port: ${port_directive}"
echo " log file: ${log_directive}"
echo " daemon: ${daemon_directive}"

if [ ! -z ${alice_directory} ]; then
  cd ${alice_directory}
fi

erl -pa $PWD/ebin -pa $PWD/deps/*/ebin ${name_directive} -boot alice -alice ${port_directive} ${log_directive} ${daemon_directive} ${rabbithost_directive}