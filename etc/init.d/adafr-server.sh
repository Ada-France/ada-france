#!/bin/sh

### BEGIN INIT INFO
# Provides:	  adafr-server
# Required-Start:    $local_fs $remote_fs $network $syslog $named $apache2
# Required-Stop:     $local_fs $remote_fs $network $syslog $named $apache2
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: starts the Ada France Web server
# Description:       starts the Ada France Web server
### END INIT INFO

NAME=adafr
ADAFR_DIR=/var/lib/$NAME
ADAFR_USER=root

# Read configuration variable file if it is present
[ -r /etc/default/$NAME ] && . /etc/default/$NAME

DAEMON=daemon

start_server() {

  $DAEMON --name $NAME --respawn --delay=60 --inherit --user $ADAFR_USER \
      --pidfile /var/run/$NAME.pid --output /var/log/$NAME.log $ADAFR_DIR/adafr-server.sh
}

stop_server() {
  $DAEMON --name $NAME --stop --pidfile /var/run/$NAME.pid
}

case $1 in
  stop)
     stop_server
     ;;
  start)
     start_server
     ;;
  restart)
     stop_server
     start_server
     ;;
  *)
     echo "Usage: adafr-server.sh [start|stop|restart]"
     ;;
esac

